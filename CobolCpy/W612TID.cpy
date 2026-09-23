000100 01  W612TID.                                                             
000200*                                 INDATA FÖR SUBPROGRAMMET                
000300*                                 W612TIME                                
000400     03 IDDC                 PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 TIINLMOT             PIC S9(7)           COMP-3.                  
000700*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
000800     03 TIINLMTI             PIC 9(4).                                    
000900*                                 MOTTAGNINGSTID     (TTMM)               
001000     03 TIINLINL             PIC S9(7)           COMP-3.                  
001100*                                 RAPPORTERINGSDATUM INLAGD (R32)         
001200     03 TIINLITI             PIC 9(4).                                    
001300*                                 RAPPORTERINGSTID   INLAGD (R32)         
001400     03 KDSVAR               PIC X.                                       
001500*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001600     03 KVDAGDEC             PIC S9(4)V9(1)      COMP-3.                  
001700*                                 ANTAL DAGAR MED DECIMAL                 
001800*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
