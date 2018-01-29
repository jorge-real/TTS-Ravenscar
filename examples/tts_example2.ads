private with STM32.GPIO, STM32.Device;

package TTS_Example2 is
   procedure Main;
private
   use STM32.GPIO, STM32.Device;
   Probe_TT_Point : GPIO_Point := PD2;
   Probe_ET_Point : GPIO_Point := PD6;
end TTS_Example2;
