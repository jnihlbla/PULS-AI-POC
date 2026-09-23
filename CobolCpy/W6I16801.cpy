000100 01  MID-W6I16801.                                                        
000200*                                 MID-COPYTEXT FÖR W6016800               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-KVANTAL-ETIK     PIC 9(6).                                    
000800*                                 ANTAL BESTÄLLDA ETIKETTER TILL          
000900*                                 UTSKRIFT                                
001000     03 MID-IDPRTLST         PIC X(8).                                    
001100*                                 LOGISK PRINTER+LISTA IDENTITET          
001200     03 MID-BEARTURS-JUST    PIC X(15).                                   
001300*                                 JUSTERAT URPRUNGSLAND                   
001400*                                 FÖR EN ARTIKEL                          
001500     03 MID-KVQPACK-JUST     PIC 9(5).                                    
001600*                                 JUSTERAD KVANT                          
001700     03 MID-IDBATCH          PIC X(18).                                   
001800*                                 ID FÖR LEVERENÖRS BATCH UPPBYGD         
001900*                                 ENLIGT FÖLJANDE:                        
002000*                                  SIFFRA 1-5       LEV.NR                
002100*                                  SIFFRA 6           0                   
002200*                                  SIFFRA 7-18      LEV. PARTINR.         
002300*                                                                         
002400*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
