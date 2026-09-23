000100 01  MOD-W4O32401.                                                        
000200*                                 MODCOPYTEXT TILL W40324.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDPRCGRP-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-KDPRCGRP-UT      PIC X(5).                                    
001000*                                 PRODUKTIONSKANALSGRUPP                  
001100     03 MOD-IDDISTR-IN       PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDDISTR-UT       PIC Z(3)9.                                   
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDKUNDNR-UT      PIC Z(5)9.                                   
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDORDNR-IN       PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDORDNR-UT       PIC Z(4)9.                                   
002200*                                 ORDERNUMMER                             
002300     03 MOD-KDORDKL-IN       PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-KDORDKL-UT       PIC Z.                                       
002600*                                 ORDERKLASS                              
002700     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900     03 MOD-IDPRODNR-UT      PIC Z(6)9.                                   
003000*                                 PRODUKTIONSNUMMER                       
003100     03 MOD-IDDC-IN          PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300     03 MOD-IDDC-UT          PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MOD-TIRFS-NYCKEL     PIC 9(11).                                   
003600*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
003700     03 MOD-IDPRODNR-NYCKEL  PIC 9(7).                                    
003800*                                 PRODUKTIONSNUMMER                       
003900     03 MOD-RAD              OCCURS 13 TIMES.                             
004000*                                                                         
004100        05 MOD-KDCMD-ATTR    PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-KDCMD         PIC X.                                       
004400*                                 RAD-UPPDATERINGSKOMMANDO                
004500*                                  BLANK  = INGENTING                     
004600*                                  D , B  = DELETE                        
004700*                                  R , Ä  = REPLACE                       
004800*                                  I,N,A  = INSERT                        
004900*                                  S , V  = SELECT                        
005000*                                  P , P  = PRINT                         
005100*                                  C , K  = COPY                          
005200        05 MOD-TIBEGPAC      PIC 9(5).                                    
005300*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
005400        05 MOD-KDORDKL       PIC 9.                                       
005500*                                 ORDERKLASS                              
005600        05 MOD-IDDISTR       PIC Z(3)9.                                   
005700*                                 DISTRIKTNUMMER                          
005800        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
005900*                                 KUNDNUMMER                              
006000        05 MOD-IDORDNR       PIC Z(4)9.                                   
006100*                                 ORDERNUMMER                             
006200        05 MOD-KDFRAKT       PIC Z9.                                      
006300*                                 FRAKTSÄTT DC TILL KUND                  
006400        05 MOD-IDPRODNR      PIC Z(6)9.                                   
006500*                                 PRODUKTIONSNUMMER                       
006600        05 MOD-KVORDRAD      PIC Z(4)9.                                   
006700*                                 ANTAL ORDERRADER                        
006800        05 MOD-KVORDRAD-PACK PIC Z(4)9.                                   
006900*                                 ANTAL PACKADE ORDERRADER                
007000        05 MOD-KVKOLPAC      PIC Z(3)9.                                   
007100*                                 ANTAL PACK RAPPORTERADE KOLLI           
007200        05 MOD-TIUTSKR       PIC 9(5).                                    
007300*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
007400        05 MOD-TISTADAT      PIC 9(5).                                    
007500*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
007600     03 MOD-TEMFSINF         PIC X(55).                                   
007700*                                 INFORMATIONSMEDDELANDE                  
007800*** END OF VILMAII-COPY LENGTH= 902 BYTES                                 
