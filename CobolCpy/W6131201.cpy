000100 01  W6131201.                                                            
000200*                                 VECKANS INLEVERANSER TILL CDC           
000300     03 BEFT                 PIC S9(3)           COMP-3.                  
000400*                                 FÖRPACKNINGSTYP                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 KVINLART             PIC S9(7)           COMP-3.                  
000800*                                 ANTAL I PARTIRAD                        
000900     03 KVAVIS-KIT           PIC S9(7)           COMP-3.                  
001000*                                 AVISERAT ANTAL FÖR SATS                 
001100     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
001200*                                 ANTAL I Q1 FÖRPACKNING                  
001300     03 KDFORP.                                                           
001400*                                 FÖRPACKNINGSKOD                         
001500        05 KDFORPPL          PIC 9.                                       
001600*                                 FÖRPACKNINGSPLATS                       
001700        05 KDFORPGP          PIC 9(2).                                    
001800*                                 FÖRPACKNINGSGRUPP                       
001900        05 KDFORPUF          PIC 9.                                       
002000*                                 UPPRÄKNINGSFAKTOR                       
002100*** END OF VILMAII-COPY LENGTH= 19 BYTES                                  
