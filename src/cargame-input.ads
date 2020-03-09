with Glfw.Input; use Glfw.Input;

package Mouse is
        Left_Button_State  : Button_State;
        Right_Button_State : Button_State;
        function  Get_Position return Vector2;
        procedure Set_Position(V : in Vector2);
private
        Position : Vector2;
end Mouse;
