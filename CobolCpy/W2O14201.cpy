000100 01  MOD-W2O14201-CTX.                                                    
000200*                                 MOD-COPYTEXT FÖR W2014200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDANSK-FOM-IN    PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDANSK-FOM-UT    PIC X(3).                                    
001000*                                 ANSKAFFARNUMMER                         
001100     03 MOD-IDANSK-TOM-IN    PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDANSK-TOM-UT    PIC X(3).                                    
001400*                                 ANSKAFFARNUMMER                         
001500     03 MOD-IDPROJ-IN        PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDPROJ-UT        PIC X(4).                                    
001800*                                 PARTS PROJEKTIDENTITET                  
001900     03 MOD-FLPISK-ENTER     PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-FLPISK-PF8       PIC X(2).                                    
002200*                                 MFS BEHANDLING AV INPUTFÄLT             
002300     03 MOD-TIFINLEV-ENTER   PIC 9(6).                                    
002400*                                 PUBLICERINGSDATUM  (AAMMDD)             
002500     03 MOD-TIFINLEV-PF8     PIC 9(6).                                    
002600*                                 PUBLICERINGSDATUM  (AAMMDD)             
002700     03 MOD-IDAO-ENTER       PIC X(10).                                   
002800*                                 ÄNDRINGSORDERNUMMER                     
002900     03 MOD-IDAO-PF8         PIC X(10).                                   
003000*                                 ÄNDRINGSORDERNUMMER                     
003100     03 MOD-IDARTNR-ENTER    PIC Z(7)9.                                   
003200*                                 ARTIKELNUMMER                           
003300     03 MOD-IDARTNR-PF8      PIC Z(7)9.                                   
003400*                                 ARTIKELNUMMER                           
003500     03 MOD-IDARTNR-WDD2B1   PIC X(9).                                    
003600*                                 ARTIKELNUMMER                           
003700     03 MOD-ANT-ART-WDD2B1-ATTR                                           
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-ANT-ART-WDD2B1   PIC X(5).                                    
004100*                                 ANTAL ARTIKLAR PER BRYTBEGREPP          
004200     03 MOD-W2O14201-001-GRP OCCURS 12 TIMES.                             
004300*                                 RADINFORMATION                          
004400        05 MOD-IDARTNR       PIC Z(7)9.                                   
004500*                                 ARTIKELNUMMER                           
004600        05 MOD-FILLERX2      PIC X(2).                                    
004700        05 MOD-IDPROJ        PIC X(4).                                    
004800*                                 PARTS PROJEKTIDENTITET                  
004900        05 MOD-FILLERX1      PIC X.                                       
005000        05 MOD-IDPROJK       PIC X(4).                                    
005100*                                 PROJEKTIDENTITET KONSTRUKTION           
005200        05 MOD-FILLERX3      PIC X(3).                                    
005300        05 MOD-IDPROJUP      PIC X(8).                                    
005400*                                 PROJEKTUPPDRAG                          
005500        05 MOD-FILLERX1      PIC X.                                       
005600        05 MOD-TEKOPTYP      PIC X(10).                                   
005700*                                 TYP AV INKÖP                            
005800        05 MOD-FILLERX1      PIC X.                                       
005900        05 MOD-TIFINLEV      PIC 9(6).                                    
006000*                                 PUBLICERINGSDATUM  (AAMMDD)             
006100        05 MOD-FILLERX4      PIC X(4).                                    
006200        05 MOD-FLPISK        PIC X.                                       
006300*                                 PISK ARTIKEL                            
006400        05 MOD-FILLERX4      PIC X(4).                                    
006500        05 MOD-TIINKOP       PIC 9(6).                                    
006600*                                 DATUM NÄR INKÖP BEGÄRS                  
006700        05 MOD-FILLERX1      PIC X.                                       
006800        05 MOD-IDINK         PIC X(4).                                    
006900*                                 INKÖPARNUMMER                           
007000        05 MOD-FILLERX2      PIC X(2).                                    
007100        05 MOD-TIMOTSI       PIC 9(6).                                    
007200*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
007300     03 MOD-IDANSK-ENTER     PIC Z(2)9.                                   
007400*                                 ANSKAFFARNUMMER                         
007500     03 MOD-IDANSK-PF8       PIC Z(2)9.                                   
007600*                                 ANSKAFFARNUMMER                         
007700     03 MOD-IDPROJ-ENTER     PIC X(4).                                    
007800*                                 PARTS PROJEKTIDENTITET                  
007900     03 MOD-IDPROJ-PF8       PIC X(4).                                    
008000*                                 PARTS PROJEKTIDENTITET                  
008100     03 MOD-TEMFSINF         PIC X(55).                                   
008200*                                 INFORMATIONSMEDDELANDE                  
008300*** END OF VILMAII-COPY LENGTH= 1109 BYTES                                
