000100 01  MOD-W2O38201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2038200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN-ATTR  PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-BEART            PIC X(22).                                   
001400     03 MOD-IDDC-IN          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-IDDC-UT          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-IDTYPE-IN        PIC X.                                       
001900     03 MOD-IDTYPE-UT        PIC X(5).                                    
002000     03 MOD-IDSTATUS-IN      PIC X.                                       
002100     03 MOD-IDSTATUS-UT      PIC X(12).                                   
002200     03 MOD-ANT-REVIEW       PIC Z(4)9.                                   
002300     03 MOD-IDREFTYP-IN      PIC X.                                       
002400*                                 TYP AV REFILLORDER                      
002500     03 MOD-IDREFTYP-UT      PIC X(5).                                    
002600     03 MOD-IDDC-TO-GRP      OCCURS 6 TIMES.                              
002700        05 MOD-IDDC-TO       PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900        05 MOD-AREA          PIC X(4).                                    
003000     03 MOD-ORDERSTATUS      PIC X(20).                                   
003100     03 MOD-GRP-RULL         OCCURS 5 TIMES.                              
003200        05 MOD-TIPP          PIC X(2).                                    
003300*                                 PLANERINGSPERIOD (PP)                   
003400*                                 12 PER ÅR                               
003500        05 MOD-TIVV-FOM-TOM  PIC X(5).                                    
003600        05 MOD-KVOI-RULL     OCCURS 6 TIMES                               
003700                             PIC Z(6)9.                                   
003800*                                 ORDERINGÅNG TILL SDC                    
003900     03 MOD-VECKA            PIC 9.                                       
004000     03 MOD-KVOI-INNEV       OCCURS 6 TIMES                               
004100                             PIC Z(6)9.                                   
004200*                                 ORDERINGÅNG TILL DC INNEV PER           
004300     03 MOD-KVPB-REF-GRP     OCCURS 6 TIMES.                              
004400        05 MOD-KVPB-REF-ATTR PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-KVPB-REF      PIC X(7).                                    
004700     03 MOD-BALANCE          OCCURS 6 TIMES                               
004800                             PIC -(6)9.                                   
004900*                                 LAGERSALDO                              
005000     03 MOD-IN-TRANS         OCCURS 6 TIMES                               
005100                             PIC Z(6)9.                                   
005200*                                 DEL AV AK SOM LIGGER I SDC              
005300     03 MOD-IDDC-TO-GRP      OCCURS 6 TIMES.                              
005400        05 MOD-KVREFPKT-ATTR PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-KVREFPKT      PIC Z(5)9.                                   
005700     03 MOD-KVREFBER         OCCURS 6 TIMES                               
005800                             PIC Z(6)9.                                   
005900*                                 BERÄKNAD REFILLINGKVANTITET             
006000     03 MOD-TIREFEFT         OCCURS 6 TIMES                               
006100                             PIC 9(6).                                    
006200*                                 DATUM SENAST EFTERFRÅGAD                
006300     03 MOD-TIINLINL         OCCURS 6 TIMES                               
006400                             PIC 9(6).                                    
006500*                                 RAPPORTERINGSDATUM INLAGD (R32)         
006600     03 MOD-PURCHQTY-GRP     OCCURS 6 TIMES.                              
006700        05 MOD-PURCHQTY-ATTR PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-PURCHQTY      PIC X(7).                                    
007000     03 MOD-IDDC-FROM-GRP    OCCURS 6 TIMES.                              
007100        05 MOD-IDDC-FROM-ATTR                                             
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-IDDC-FROM     PIC X(2).                                    
007500*                                 IDENTIFIERARE LAGER                     
007600     03 MOD-MODEL            OCCURS 3 TIMES                               
007700                             PIC X(4).                                    
007800     03 MOD-TIFINLV          PIC 9(5).                                    
007900*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
008000     03 MOD-TIURPROD         PIC 9(4).                                    
008100*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
008200     03 MOD-REPLACES         PIC X(9).                                    
008300*                                 ARTIKELNUMMER                           
008400     03 MOD-REPL-BY          PIC X(9).                                    
008500*                                 ARTIKELNUMMER                           
008600     03 MOD-KDERS            PIC Z9.                                      
008700*                                 ERSÄTTNINGSKOD                          
008800     03 MOD-KDPRODSL         PIC Z9.                                      
008900*                                 PRODUKTSLAG                             
009000     03 MOD-VLARTNTO         PIC Z(7)9.9.                                 
009100*                                 ARTIKELVOLYM (CM3)                      
009200     03 MOD-PRICE-TYPE       PIC X(6).                                    
009300     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
009400*                                 ARTIKELSTANDARDPRIS                     
009500     03 MOD-KVPB-CDC         PIC Z(6)9.9.                                 
009600*                                 PERIODBEHOV (PROGNOS)                   
009700     03 MOD-AVAIL            PIC -(7)9.                                   
009800*                                 LAGERSALDO                              
009900     03 MOD-KVROS-CDC        PIC -(6)9.                                   
010000*                                 RESTORDERSALDO                          
010100     03 MOD-FLREFBEO-GRP     OCCURS 6 TIMES.                              
010200        05 MOD-FLREFBEO-ATTR PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 MOD-FLREFBEO      PIC X(2).                                    
010500*                                 MFS BEHANDLING AV INPUTFÄLT             
010600     03 MOD-SEASON           OCCURS 6 TIMES                               
010700                             PIC X.                                       
010800     03 MOD-KVQPACK-1        PIC Z(4)9.                                   
010900*                                 ANTAL I Q1 FÖRPACKNING                  
011000     03 MOD-KVQPACK-3        PIC Z(4)9.                                   
011100*                                 ANTAL I Q3 FÖRPACKNING                  
011200     03 MOD-COMMENT-GRP      OCCURS 2 TIMES.                              
011300        05 MOD-COMMENT-ATTR  PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500        05 MOD-COMMENT       PIC X(15).                                   
011600     03 MOD-TEMFSINF         PIC X(72).                                   
011700*                                 INFORMATIONSMEDDELANDE                  
011800*** END OF VILMAII-COPY LENGTH= 1081 BYTES                                
