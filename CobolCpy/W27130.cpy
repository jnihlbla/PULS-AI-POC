000100 01  W27130-CTX.                                                          
000200*                                 COPYTEXT TILL FILEN W27130,             
000300*                                 ARTIKLAR VARS PROGNOS SKALL             
000400*                                 UPPDATERAS                              
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
001000*                                 PERIODBEHOV REFILLING                   
001100     03 RETREND              PIC S9(2)V9(1)      COMP-3.                  
001200*                                 KVOT PREL-PROGNOS MEDELPROGNOS          
001300     03 KDERS                PIC S9(3)           COMP-3.                  
001400*                                 ERSÄTTNINGSKOD                          
001500     03 TIREFMPB             PIC S9(7)           COMP-3.                  
001600*                                 DATUM MANUELL PROGNOS REFILLING         
001700     03 NOLLA-JUST1          PIC X.                                       
001800     03 NOLLA-JUST2          PIC X.                                       
001900*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
