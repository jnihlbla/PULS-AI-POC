000100 01  MOD-W2O36801.                                                        
000200*                                 MOD-COPYTEXT FÖR W2036800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-COPY-IDDC-ATTR   PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-COPY-IDDC        PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-KVOT-UT          PIC Z(6)9.                                   
001600*                                 ANTAL ORDERTRÄFF                        
001700     03 MOD-KVOT-IN-ATTR     PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-KVOT-IN          PIC Z(6)9.                                   
002000*                                 ANTAL ORDERTRÄFF                        
002100     03 MOD-KVPB-LIM-HF-UT   PIC Z(5)9.9.                                 
002200*                                 GRÄNS PERIODBEHOV HÖG FREKVENT          
002300     03 MOD-KVPB-LIM-HF-IN-ATTR                                           
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-KVPB-LIM-HF-IN   PIC Z(5)9.9.                                 
002700*                                 GRÄNS PERIODBEHOV HÖG FREKVENT          
002800     03 MOD-KVPB-LIM-LF-UT   PIC Z(5)9.9.                                 
002900*                                 GRÄNS PERIODBEHOV LÅG FREKVENT          
003000     03 MOD-KVPB-LIM-LF-IN-ATTR                                           
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-KVPB-LIM-LF-IN   PIC Z(5)9.9.                                 
003400*                                 GRÄNS PERIODBEHOV LÅG FREKVENT          
003500     03 MOD-IDUSER           PIC X(8).                                    
003600*                                 ANVÄNDARENS SÄKERHETS ID                
003700     03 MOD-TIUPPDAT         PIC 9(6).                                    
003800*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003900     03 MOD-GRP              OCCURS 12 TIMES.                             
004000        05 MOD-SELECT        PIC X.                                       
004100        05 MOD-IDPERSON-BUY  PIC Z(2)9.                                   
004200*                                 PERSONKOD REFILLANSVARIG                
004300        05 MOD-BEBUYER       PIC X(35).                                   
004400*                                  BUYER BENÄMNING                        
004500        05 MOD-IDREFTAB-LF-UT                                             
004600                             PIC X.                                       
004700*                                 ID REFILLTABELL LÅG FREKVENT            
004800        05 MOD-IDREFTAB-LF-IN-ATTR                                        
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-IDREFTAB-LF-IN                                             
005200                             PIC X.                                       
005300*                                 ID REFILLTABELL LÅG FREKVENT            
005400        05 MOD-IDREFTAB-HF-UT                                             
005500                             PIC X.                                       
005600*                                 ID REFILLTABELL HÖG FREKVENT            
005700        05 MOD-IDREFTAB-HF-IN-ATTR                                        
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-IDREFTAB-HF-IN                                             
006100                             PIC X.                                       
006200*                                 ID REFILLTABELL HÖG FREKVENT            
006300     03 MOD-NEW-IDPERSON-BUY-ATTR                                         
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-NEW-IDPERSON-BUY PIC Z(2)9.                                   
006700*                                 PERSONKOD REFILLANSVARIG                
006800     03 MOD-NEW-BEBUYER-ATTR PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-NEW-BEBUYER      PIC X(35).                                   
007100*                                  BUYER BENÄMNING                        
007200     03 MOD-NEW-IDREFTAB-LF-ATTR                                          
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-NEW-IDREFTAB-LF  PIC X.                                       
007600*                                 ID REFILLTABELL LÅG FREKVENT            
007700     03 MOD-NEW-IDREFTAB-HF-ATTR                                          
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-NEW-IDREFTAB-HF  PIC X.                                       
008100*                                 ID REFILLTABELL HÖG FREKVENT            
008200     03 MOD-TEMFSINF         PIC X(55).                                   
008300*                                 INFORMATIONSMEDDELANDE                  
008400*** END OF VILMAII-COPY LENGTH= 785 BYTES                                 
