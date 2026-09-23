000100 01  MOD-W4O40801.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDDC-REF-IN      PIC X(2).                                    
001200*                                 SÄNDANDE LAGER FÖR REFILL               
001300     03 MOD-IDDC-REF-UT      PIC X(2).                                    
001400*                                 SÄNDANDE LAGER FÖR REFILL               
001500     03 MOD-IDUSER           PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700     03 MOD-TIUPPDAT         PIC 9(6).                                    
001800*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001900     03 MOD-KVDLTID-AIRPAC   PIC Z9.                                      
002000*                                 ANTAL ARB.DAGAR PACK.TID,  FLYG         
002100     03 MOD-KVDLTID-AIRPAC-IN-ATTR                                        
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-KVDLTID-AIRPAC-IN                                             
002500                             PIC Z9.                                      
002600*                                 ANTAL ARB.DAGAR PACK.TID,  FLYG         
002700     03 MOD-KVDLTID-BOATPAC  PIC Z9.                                      
002800*                                 ANTAL ARBDAGAR PACKNINGSTID,BÅT         
002900     03 MOD-KVDLTID-BOATPAC-IN-ATTR                                       
003000                             PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-KVDLTID-BOATPAC-IN                                            
003300                             PIC Z9.                                      
003400*                                 ANTAL ARBDAGAR PACKNINGSTID,BÅT         
003500     03 MOD-KVDLTID-AIRTRP   PIC Z9.                                      
003600*                                 KALENDERDAGAR TRANSPORTTID,FLYG         
003700     03 MOD-KVDLTID-AIRTRP-IN-ATTR                                        
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-KVDLTID-AIRTRP-IN                                             
004100                             PIC Z9.                                      
004200*                                 KALENDERDAGAR TRANSPORTTID,FLYG         
004300     03 MOD-KVDLTID-BOATTRP  PIC Z9.                                      
004400*                                 KALENDERDAGAR TRANSPORTTID, BÅT         
004500     03 MOD-KVDLTID-BOATTRP-IN-ATTR                                       
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-KVDLTID-BOATTRP-IN                                            
004900                             PIC Z9.                                      
005000*                                 KALENDERDAGAR TRANSPORTTID, BÅT         
005100     03 MOD-KVDLTID-BOAT2DC  PIC Z9.                                      
005200*                                 ARB.DAGAR FRÅN HAMN TILL DC,BÅT         
005300     03 MOD-KVDLTID-BOAT2DC-IN-ATTR                                       
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-KVDLTID-BOAT2DC-IN                                            
005700                             PIC Z9.                                      
005800*                                 ARB.DAGAR FRÅN HAMN TILL DC,BÅT         
005900     03 MOD-KVDLTID-AIRINS   PIC Z9.                                      
006000*                                 ANTAL ARBDAGAR INLÄGGNING, FLYG         
006100     03 MOD-KVDLTID-AIRINS-IN-ATTR                                        
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-KVDLTID-AIRINS-IN                                             
006500                             PIC Z9.                                      
006600*                                 ANTAL ARBDAGAR INLÄGGNING, FLYG         
006700     03 MOD-KVDLTID-BOATINS  PIC Z9.                                      
006800*                                 ANTAL ARBDAGAR INLÄGG.TID,  BÅT         
006900     03 MOD-KVDLTID-BOATINS-IN-ATTR                                       
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-KVDLTID-BOATINS-IN                                            
007300                             PIC Z9.                                      
007400*                                 ANTAL ARBDAGAR INLÄGG.TID,  BÅT         
007500     03 MOD-KVDLTID-BUFF     PIC Z9.                                      
007600*                                 ARBETSDAGAR LEDTID BUFFER               
007700     03 MOD-KVDLTID-BUFF-IN-ATTR                                          
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-KVDLTID-BUFF-IN  PIC Z9.                                      
008100*                                 ANTAL ARBDAGAR INLÄGG.TID,  BÅT         
008200     03 MOD-AIRSHLF          PIC 9(6).                                    
008300     03 MOD-BOATSHLF         PIC 9(6).                                    
008400     03 MOD-BUFFSHLF         PIC 9(6).                                    
008500     03 MOD-KVDLTID-CUST     PIC Z9.                                      
008600*                                 ARBETSDAGAR LEDTID I TULLEN             
008700     03 MOD-KVDLTID-CUST-IN-ATTR                                          
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-KVDLTID-CUST-IN  PIC Z9.                                      
009100*                                 ARBETSDAGAR LEDTID I TULLEN             
009200     03 MOD-KVDLTID-CUSTWAIT PIC Z9.                                      
009300*                                 ARBETSDAGAR VÄNTETID I TULLEN           
009400     03 MOD-KVDLTID-CUSTWAIT-IN-ATTR                                      
009500                             PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700     03 MOD-KVDLTID-CUSTWAIT-IN                                           
009800                             PIC Z9.                                      
009900*                                 ARBETSDAGAR VÄNTETID I TULLEN           
010000     03 MOD-KVDLTID-CUST2DC  PIC Z9.                                      
010100*                                 ARBETSDAGAR FRÅN TULL TILL DC           
010200     03 MOD-KVDLTID-CUST2DC-IN-ATTR                                       
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 MOD-KVDLTID-CUST2DC-IN                                            
010600                             PIC Z9.                                      
010700*                                 ARBETSDAGAR FRÅN TULL TILL DC           
010800     03 MOD-KVDLTID-AIRETA   PIC Z9.                                      
010900*                                 KAL.DAGAR FÖR ANKONSTDATUM FLYG         
011000     03 MOD-KVDLTID-AIRETA-IN-ATTR                                        
011100                             PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300     03 MOD-KVDLTID-AIRETA-IN                                             
011400                             PIC Z9.                                      
011500*                                 KAL.DAGAR FÖR ANKONSTDATUM FLYG         
011600     03 MOD-KVDLTID-TOT      PIC Z(2)9.                                   
011700*                                 TOT ANTAL KALENDERDAGAR LEDTID          
011800     03 MOD-KVDLTID-TOT-IN-ATTR                                           
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 MOD-KVDLTID-TOT-IN   PIC Z(2)9.                                   
012200*                                 TOT ANTAL KALENDERDAGAR LEDTID          
012300     03 MOD-TIREFBAT         PIC 9(2).                                    
012400*                                 KLOCKSLAG (TT) FÖR REFILL BATCH         
012500     03 MOD-TIREFBAT-IN-ATTR PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-TIREFBAT-IN      PIC 9(2).                                    
012800*                                 KLOCKSLAG (TT) FÖR REFILL BATCH         
012900     03 MOD-REAIRCO          PIC Z(4)9.9.                                 
013000*                                 KONSTANT FLYGFRAKT BERÄKNING            
013100     03 MOD-REAIRCO-IN-ATTR  PIC X(2).                                    
013200*                                 MFS ATTRIBUTFÄLT                        
013300     03 MOD-REAIRCO-IN       PIC Z(4)9.9.                                 
013400*                                 KONSTANT FLYGFRAKT BERÄKNING            
013500     03 MOD-KVDLTID-AIRREQ   PIC Z9.                                      
013600*                                 ANTAL KALENDERDAGAR BEHOV, FLYG         
013700     03 MOD-KVDLTID-AIRREQ-IN-ATTR                                        
013800                             PIC X(2).                                    
013900*                                 MFS ATTRIBUTFÄLT                        
014000     03 MOD-KVDLTID-AIRREQ-IN                                             
014100                             PIC Z9.                                      
014200*                                 ANTAL KALENDERDAGAR BEHOV, FLYG         
014300     03 MOD-RESSFAC          PIC 9.9(2).                                  
014400*                                 KONSTANT SÄKERHETSLAGER BERÄKNI         
014500*                                 NG                                      
014600     03 MOD-RESSFAC-IN-ATTR  PIC X(2).                                    
014700*                                 MFS ATTRIBUTFÄLT                        
014800     03 MOD-RESSFAC-IN       PIC 9.9(2).                                  
014900*                                 KONSTANT SÄKERHETSLAGER BERÄKNI         
015000*                                 NG                                      
015100     03 MOD-PRFRAKT          PIC Z(6)9.                                   
015200*                                 FRAKTKOSTNAD                            
015300     03 MOD-PRFRAKT-IN-ATTR  PIC X(2).                                    
015400*                                 MFS ATTRIBUTFÄLT                        
015500     03 MOD-PRFRAKT-IN       PIC Z(6)9.                                   
015600*                                 FRAKTKOSTNAD                            
015700     03 MOD-BETEXT           PIC X(55).                                   
015800     03 MOD-TEMFSINF         PIC X(55).                                   
015900*                                 INFORMATIONSMEDDELANDE                  
016000*** END OF VILMAII-COPY LENGTH= 328 BYTES                                 
