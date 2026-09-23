000100 01  MOD-W2O12401.                                                        
000200*                                 COPYTEXT FÖR MOD W2O12401               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-SPARADE-NYCKLAR.                                              
001200*                                       SPARADE NYCKLAR PF8               
001300        05 MOD-IDDC-SPAR     PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500        05 MOD-IDARTNR-SPAR  PIC 9(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-RAD.                                                          
001800*                                  RAD1                                   
001900*                                                                         
002000        05 MOD-KDERSTMP-ATTR PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200        05 MOD-KDERSTMP      PIC X.                                       
002300*                                 TEMPORÄR ERSÄTTNINGSREGEL               
002400        05 MOD-IDANSK        PIC 9(3).                                    
002500*                                 ANSKAFFARNUMMER                         
002600        05 MOD-KVLS          PIC Z(6)9.                                   
002700*                                 LAGERSALDO                              
002800        05 MOD-KVAKS         PIC Z(6)9.                                   
002900*                                 ANKOMSTSALDO                            
003000        05 MOD-KVRO          PIC Z(5)9.                                   
003100*                                 ANTAL RESTNOTERADE ARTIKLAR             
003200        05 MOD-SUROBEL       PIC Z(6)9.                                   
003300*                                 RESTORDERVÄRDE STANDARDPRIS             
003400        05 MOD-KDERS         PIC 9(2).                                    
003500*                                 ERSÄTTNINGSKOD                          
003600     03 MOD-RADER            OCCURS 8 TIMES                               
003700                             INDEXED MOD-IX-1.                            
003800*                                  RADER                                  
003900*                                                                         
004000        05 MOD-IDDC-RADER    PIC X(2).                                    
004100*                                 IDENTIFIERARE LAGER                     
004200        05 MOD-KVAKS-PAV-RADER                                            
004300                             PIC Z(6)9.                                   
004400*                                 DEL AV AK PÅ VÄG                        
004500        05 MOD-SUAKSV-PAV-RADER                                           
004600                             PIC Z(6)9.                                   
004700*                                 AK PÅ VÄG VÄRDE                         
004800        05 MOD-TIAVIDAT-RADER                                             
004900                             PIC X(6).                                    
005000*                                 AVISERINGSDATUM (YYMMDD)                
005100        05 MOD-KVLS-RADER    PIC Z(6)9.                                   
005200*                                 LAGERSALDO                              
005300        05 MOD-SULV-RADER    PIC Z(8)9.                                   
005400*                                 LAGERVÄRDE                              
005500     03 MOD-EOP.                                                          
005600*                                  EOP                                    
005700*                                                                         
005800        05 MOD-FLFORTS       PIC X.                                       
005900*                                 FÖRSTA GGN =N, FORSTÄTTNING =J          
006000        05 MOD-SULV-TOT      PIC Z(8)9.                                   
006100*                                 LAGERVÄRDE                              
006200        05 MOD-IDUSER        PIC X(8).                                    
006300*                                 ANVÄNDARENS SÄKERHETS ID                
006400        05 MOD-DAREGDAT      PIC X(6).                                    
006500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006600        05 MOD-BEKOM-ATTR    PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-BEKOM         PIC X(50).                                   
006900*                                 KOMMENTAR/INSTRUKTION                   
007000     03 MOD-LOGG             OCCURS 14 TIMES                              
007100                             INDEXED MOD-IX-1.                            
007200*                                  LOGG                                   
007300*                                                                         
007400        05 MOD-DAREGDAT-LOGG PIC X(6).                                    
007500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007600        05 MOD-KDERSTMP-LOGG PIC X.                                       
007700*                                 TEMPORÄR ERSÄTTNINGSREGEL               
007800        05 MOD-SULV-LOGG     PIC Z(8)9.                                   
007900*                                 LAGERVÄRDE                              
008000        05 MOD-IDUSER-LOGG   PIC X(8).                                    
008100*                                 ANVÄNDARENS SÄKERHETS ID                
008200     03 MOD-TEMFSINF         PIC X(55).                                   
008300*                                 INFORMATIONSMEDDELANDE                  
008400*** END OF VILMAII-COPY LENGTH= 879 BYTES                                 
