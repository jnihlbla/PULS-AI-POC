000100 01  MID-W4I41201.                                                        
000200*                                                                         
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-FLDISTCUST-IN    PIC X.                                       
001200     03 MID-FLDISTCUST-UT    PIC X.                                       
001300     03 MID-INPUT1.                                                       
001400*                                                                         
001500        05 MID-FLAUTORD      PIC X.                                       
001600*                                 FLAGGAN STYR OM AUTOMATORDER-           
001700*                                 NUMMER SKALL SKAPAS                     
001800        05 MID-IDRFTAB       PIC X(3).                                    
001900*                                 RANSONERINGSFAKTORTABELL                
002000        05 MID-FLNC          PIC X.                                       
002100*                                 NEW CONCEPT FLAGGA                      
002200        05 MID-KDORDING      PIC 9.                                       
002300*                                 UPPDATERING ORDERINGÅNG                 
002400        05 MID-FLVR          PIC X.                                       
002500*                                 ANSLUTEN TILL VR-SYST                   
002600        05 MID-KVVECKOR-OB-IN                                             
002700                             PIC X(3).                                    
002800*                                 ANTAL VECKOR                            
002900        05 MID-FLURSRAP      PIC X.                                       
003000*                                 URSPRUNGSRAPPORTERING VID               
003100*                                 PACKNING                                
003200        05 MID-FLSAMFAK      PIC X.                                       
003300*                                 SAMFAKTURERING NEW CONCEPT              
003400        05 MID-FLSWCONS      PIC X.                                       
003500        05 MID-FLPRERS       PIC X.                                       
003600*                                 PRISERSÄTTNINGSFLAGGA                   
003700     03 MID-INPUT2.                                                       
003800*                                                                         
003900        05 MID-FLFAKVKT      PIC X.                                       
004000*                                 VIKT ANGES I FAKTURAN                   
004100        05 MID-REAVDRAG-IN   PIC X(4).                                    
004200*                                 AVDRAGSPROCENT                          
004300        05 MID-FLFAKURS      PIC X.                                       
004400*                                 URSPRUNGLAND I FAKTURA                  
004500        05 MID-REEMBHNT-IN   PIC X(4).                                    
004600*                                 EMB OCH HANTERINGSKOST (%)              
004700        05 MID-KDSTATNR      PIC 9.                                       
004800*                                 STATNUMMER TYP                          
004900*                                 1 = NORSKT                              
005000*                                 2 = ENGELSKT                            
005100*                                 3 = BELGISKT                            
005200*                                 4 = PERUANSKT                           
005300*                                 5 = SVENSKT                             
005400*                                 6 =                                     
005500        05 MID-KDHBLKRV      PIC X.                                       
005600*                                 HANDELSBLOCK                            
005700        05 MID-KDSPRAK       PIC X.                                       
005800*                                 SPRÅKKOD                                
005900        05 MID-IDSKYLT-IN    PIC X(3).                                    
006000*                                 NATIONALITETSTECKEN                     
006100*                                 SPRÅKIDENTIFIKATION                     
006200        05 MID-IDPARTNR      PIC X(9).                                    
006300*                                 FINANCIELL KUND                         
006400        05 MID-IDFTG         PIC 9(2).                                    
006500*                                 FÖRETAGSID EKONOM REDOVISNING           
006600        05 MID-IDLEVNR-IN    PIC X(5).                                    
006700*                                 LEVERANTÖRNUMMER                        
006800     03 MID-INPUT3.                                                       
006900*                                                                         
007000        05 MID-IDDC-TVSVOR   OCCURS 16 TIMES                              
007100                             PIC X(2).                                    
007200*                                 TVÅNGSSTYRNING AV VOR-SLÄPP             
007300*                                 FRÅN DC                                 
007400     03 MID-INPUT4.                                                       
007500*                                                                         
007600        05 MID-IDDC-PREPLAN  OCCURS 8 TIMES                               
007700                             PIC X(2).                                    
007800*                                 PRE-PLANNED RELEASE TO DC               
007900*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
