package body Cargame.Engine is

   procedure Deduplicate
      (Input   : in  Vertex_Data;
       Output  : out Vertex_Data;
       Indices : out Vector_Of_Size)
   is
      subtype Attribute_Index is GL.Types.Size;

      package GL_Attributes is

         --  Inputs:
         procedure Add (V, N : in Vector3; T : in Vector2);

         --  Outputs:
         function Get_Floats  return Vector_Of_Single;
         function Get_Indices return Int_Array;

      private

         Floats          : Vector_Of_Single;
         Attrib_Sequence : Vector_Of_Size;
         --  Naming this is tricky. It IS a list of attrib ID's, but they may
         --  be repeated.

         Next_ID : Attribute_Index := 0;
         --  Indices passed to glDrawElements and such. Starts at 0.

         type Vertex_Element is record
            Start, Length : Size;
         end record;

         function Hash
            (Floats : in Vector_Of_Single;
             Elem   : in Vertex_Element)
            return Hash_Type
         is
            use Ada.Strings.Unbounded;
            Str : Unbounded_String := "";
         begin
            for I in Elem.Start .. (Elem.Start + Elem.Length) loop
               Str := @ & Floats (I)'Img;
            end loop;
            return Ada.Strings.Unbounded.Hash_Case_Insensitive (Str);
         end Hash;

         package Element_Map is new Ada.Containers.Hashed_Maps
            (Key_Type        => Vertex_Element,
             Element_Type    => Attribute_Index,
             Hash            => Hash,
             Equivalent_Keys => "=");

         use Element_Map;

         Map : Element_Map.Map;
      end GL_Attributes;

      package body GL_Attributes is

         procedure Add (V, N : Vector3; T : Vector2) is
            Elem : constant FC_Data := (V => V, T => T, N => N);
         begin
            if Map.Find (Elem) /= No_Element then
               --  Duplicate attribute, so it already has an attrib ID. Just
               --  append that ID.
               Attrib_Sequence.Append (Map.Element (Elem));
            else
               --  New attribute, so it needs an ID and to have its data added.
               Vertices.Append (V);
               Normals.Append (N);
               TexCrds.Append (T);

               pragma Assert (Vertices.Last_Index = GL_Attributes.Next_ID);
               pragma Assert (Normals.Last_Index = GL_Attributes.Next_ID);
               pragma Assert (TexCrds.Last_Index = GL_Attributes.Next_ID);

               Attrib_Sequence.Append (GL_Attributes.Next_ID);

               Map.Insert (Elem, GL_Attributes.Next_ID);

               GL_Attributes.Next_ID := GL_Attributes.Next_ID + 1;
            end if;
         end Add;

         function Get_Vertices return Vector3_Array is
            subtype Attrib_ID is Attribute_Index range
               Vertices.First_Index .. Vertices.Last_Index;
         begin
            return Ret : Vector3_Array (Attrib_ID) do
               for I in Attrib_ID loop
                  Ret (I) := Vertices (I);
               end loop;
            end return;
         end Get_Vertices;

         function Get_Normals return Vector3_Array is
            subtype Attrib_ID is Attribute_Index range
               Normals.First_Index .. Normals.Last_Index;
         begin
            return Ret : Vector3_Array (Attrib_ID) do
               for I in Attrib_ID loop
                  Ret (I) := Normals (I);
               end loop;
            end return;
         end Get_Normals;

         function Get_TexCrds return Vector2_Array is
            subtype Attrib_ID is Attribute_Index range
               TexCrds.First_Index .. TexCrds.Last_Index;
         begin
            return Ret : Vector2_Array (Attrib_ID) do
               for I in Attrib_ID loop
                  Ret (I) := TexCrds (I);
               end loop;
            end return;
         end Get_TexCrds;

         function Get_Indices return Int_Array is
            subtype Index is Attribute_Index range
               Attrib_Sequence.First_Index .. Attrib_Sequence.Last_Index;
         begin
            return Ret : Int_Array (Index) do
               for I in Index loop
                  Ret (I) := Attrib_Sequence (I);
               end loop;
            end return;
         end Get_Indices;

      end GL_Attributes;

   begin

      for I in Input.Data'First .. Num_Elements (Input) loop
         GL_Attributes.Add( Input.Data( I * Layout_Stride( Input.Layout ) ) );
      end loop;

      Output := Vertex_Data'(Data      => GL_Attributes.Get_Floats,
                             Layout    => Input.Layout,
                             Materials => Input.Materials);
      Indices := GL_Attributes.Get_Indices;

   end Deduplicate;

end Cargame.Engine;