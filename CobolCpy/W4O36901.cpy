000100 01  MOD-W4O36901.                                                        
000200*                                 MOD-COPYTEXT TILL W4036900              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPRC-IN.                                                     
000800*                                 PRODUKTIONSKANAL                        
000900        05 MOD-IDPRCBAS      PIC X(3).                                    
001000*                                 PRC-BAS                                 
001100        05 MOD-IDPRCVAR      PIC X.                                       
001200*                                 PRC-VARIANT                             
001300     03 MOD-IDPRC-UT.                                                     
001400*                                 PRODUKTIONSKANAL                        
001500        05 MOD-IDPRCBAS      PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700        05 MOD-IDPRCVAR      PIC X.                                       
001800*                                 PRC-VARIANT                             
001900     03 MOD-DATUM-IN         PIC 9(6).                                    
002000*                                 DATUM ENLIGT KDDATFORM                  
002100     03 MOD-DATUM-UT         PIC 9(6).                                    
002200*                                 DATUM ENLIGT KDDATFORM                  
002300     03 MOD-IDDC-IN          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-DATUM-NEXT       PIC 9(6).                                    
002800*                                 DATUM ENLIGT KDDATFORM                  
002900     03 MOD-DATUM-ENTER      PIC 9(6).                                    
003000*                                 DATUM ENLIGT KDDATFORM                  
003100     03 MOD-RAD              OCCURS 12 TIMES.                             
003200        05 MOD-DATUM-RAD     PIC 9(6).                                    
003300*                                 DATUM ENLIGT KDDATFORM                  
003400        05 MOD-STAPAC-RAD    PIC Z9.9(2).                                 
003500*                                 ARBETSDAGENS BÖRJAN (PACKNING)          
003600        05 MOD-STOPAC-RAD    PIC Z9.9(2).                                 
003700*                                 ARBETSDAGENS SLUT (PACKNING)            
003800        05 MOD-STAADM-RAD    PIC Z9.9(2).                                 
003900*                                 ARBETSDAGENS BÖRJAN (ORDERKONT)         
004000        05 MOD-STOADM-RAD    PIC Z9.9(2).                                 
004100*                                 ARBETSDAGENS SLUT (ORDERKONT)           
004200        05 MOD-STALAST-RAD   PIC Z9.9(2).                                 
004300*                                 ARBETSDAGENS BÖRJAN (LASTNING)          
004400        05 MOD-STOLAST-RAD   PIC Z9.9(2).                                 
004500*                                 ARBETSDAGENS SLUT (LASTNING)            
004600     03 MOD-DATUM-ATTR       PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-DATUM            PIC X(6).                                    
004900*                                 DATUM ENLIGT KDDATFORM                  
005000     03 MOD-STAPAC-IN-ATTR   PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-STAPAC-IN        PIC X(5).                                    
005300*                                 ARBETSDAGENS BÖRJAN (PACKNING)          
005400     03 MOD-STOPAC-IN-ATTR   PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-STOPAC-IN        PIC X(5).                                    
005700*                                 ARBETSDAGENS SLUT (PACKNING)            
005800     03 MOD-STAADM-IN-ATTR   PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-STAADM-IN        PIC X(5).                                    
006100*                                 ARBETSDAGENS BÖRJAN (ORDERKONT)         
006200     03 MOD-STOADM-IN-ATTR   PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-STOADM-IN        PIC X(5).                                    
006500*                                 ARBETSDAGENS SLUT (ORDERKONT)           
006600     03 MOD-STALAST-IN-ATTR  PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-STALAST-IN       PIC X(5).                                    
006900*                                 ARBETSDAGENS BÖRJAN (LASTNING)          
007000     03 MOD-STOLAST-IN-ATTR  PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-STOLAST-IN       PIC X(5).                                    
007300*                                 ARBETSDAGENS SLUT (LASTNING)            
007400     03 MOD-TEMFSINF         PIC X(55).                                   
007500*                                 INFORMATIONSMEDDELANDE                  
007600*** END OF VILMAII-COPY LENGTH= 617 BYTES                                 
