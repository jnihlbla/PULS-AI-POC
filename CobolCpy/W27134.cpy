000100 01  W27134-CTX.                                                          
000200*                                 COPYTEXT TILL FILEN W27134,             
000300*                                 ARTIKLAR VARS KVPBREOI SKALL            
000400*                                 UPPDATERAS                              
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 KVPBREOI             PIC S9(6)V9(1)      COMP-3.                  
001000*                                 PERIODBEHOV FÖR REFILL OI               
001100     03 RETREND-REOI         PIC S9(2)V9(1)      COMP-3.                  
001200*                                 KVOT PREL-PROGNOS MEDELPROGNOS          
001300     03 KDERS                PIC S9(3)           COMP-3.                  
001400*                                 ERSÄTTNINGSKOD                          
001500*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
