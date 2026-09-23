000100 01  PRDC-W510PRDC.                                                       
000200*                                 LINKAGE AREA ON CALL OF SUBPGM          
000300*                                                                         
000400*                                 W510PRDC.                               
000500*                                                                         
000600*                                                                         
000700*                                                                         
000800     03 PRDC-KDCALL          PIC S9(3)           COMP-3.                  
000900*                                 ANROPSTYP                               
001000     03 PRDC-IDLEGSEL        PIC X(4).                                    
001100*                                 FAKTURERANDE FÖRETAG TEX VCCS           
001200     03 PRDC-W510PRDC-RESP   OCCURS 50 TIMES.                             
001300        05 PRDC-IDDC         PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 PRDC-KDSVAR          PIC X.                                       
001600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001700*** END OF VILMAII-COPY LENGTH= 107 BYTES                                 
