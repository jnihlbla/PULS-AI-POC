000100 01  MID-W4I40801.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-REF-IN      PIC X(2).                                    
000600*                                 SÄNDANDE LAGER FÖR REFILL               
000700     03 MID-INPUT.                                                        
000800        05 MID-KVDLTID-AIRPAC-IN                                          
000900                             PIC 9(2).                                    
001000*                                 ANTAL ARB.DAGAR PACK.TID,  FLYG         
001100        05 MID-KVDLTID-BOATPAC-IN                                         
001200                             PIC 9(2).                                    
001300*                                 ANTAL ARBDAGAR PACKNINGSTID,BÅT         
001400        05 MID-KVDLTID-AIRTRP-IN                                          
001500                             PIC 9(2).                                    
001600*                                 KALENDERDAGAR TRANSPORTTID,FLYG         
001700        05 MID-KVDLTID-BOATTRP-IN                                         
001800                             PIC 9(2).                                    
001900*                                 KALENDERDAGAR TRANSPORTTID, BÅT         
002000        05 MID-KVDLTID-BOAT2DC-IN                                         
002100                             PIC 9(2).                                    
002200*                                 ARB.DAGAR FRÅN HAMN TILL DC,BÅT         
002300        05 MID-KVDLTID-AIRINS-IN                                          
002400                             PIC 9(2).                                    
002500*                                 ANTAL ARBDAGAR INLÄGGNING, FLYG         
002600        05 MID-KVDLTID-BOATINS-IN                                         
002700                             PIC 9(2).                                    
002800*                                 ANTAL ARBDAGAR INLÄGG.TID,  BÅT         
002900        05 MID-KVDLTID-BUFF-IN                                            
003000                             PIC 9(2).                                    
003100*                                 ARBETSDAGAR LEDTID BUFFER               
003200        05 MID-KVDLTID-CUST-IN                                            
003300                             PIC 9(2).                                    
003400*                                 ARBETSDAGAR LEDTID I TULLEN             
003500        05 MID-KVDLTID-CUSTWAIT-IN                                        
003600                             PIC 9(2).                                    
003700*                                 ARBETSDAGAR VÄNTETID I TULLEN           
003800        05 MID-KVDLTID-CUST2DC-IN                                         
003900                             PIC 9(2).                                    
004000*                                 ARBETSDAGAR FRÅN TULL TILL DC           
004100        05 MID-KVDLTID-AIRETA-IN                                          
004200                             PIC 9(2).                                    
004300*                                 KAL.DAGAR FÖR ANKONSTDATUM FLYG         
004400        05 MID-KVDLTID-TOT-IN                                             
004500                             PIC 9(3).                                    
004600*                                 TOT ANTAL KALENDERDAGAR LEDTID          
004700        05 MID-TIREFBAT-IN   PIC 9(2).                                    
004800*                                 KLOCKSLAG (TT) FÖR REFILL BATCH         
004900        05 MID-REAIRCO-IN    PIC X(7).                                    
005000*                                 KONSTANT FLYGFRAKT BERÄKNING            
005100        05 MID-KVDLTID-AIRREQ-IN                                          
005200                             PIC 9(2).                                    
005300*                                 ANTAL KALENDERDAGAR BEHOV, FLYG         
005400        05 MID-RESSFAC-IN    PIC X(4).                                    
005500*                                 KONSTANT SÄKERHETSLAGER BERÄKNI         
005600*                                 NG                                      
005700        05 MID-PRFRAKT-IN    PIC X(7).                                    
005800*                                 FRAKTKOSTNAD                            
005900     03 MID-BETEXT           PIC X(55).                                   
006000*** END OF VILMAII-COPY LENGTH= 108 BYTES                                 
