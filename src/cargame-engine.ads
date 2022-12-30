with Cargame.Util;

package Cargame.Engine is

   ------------------
   --  Scheduling  --
   ------------------

   Gameplay_Update_Done : Util.Semaphore;
   Render_Done          : Util.Semaphore;

end Cargame.Engine;