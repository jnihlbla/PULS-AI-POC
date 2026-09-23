000100 01  RESP-W90321O1-CTX.                                                   
000200*                                 COPYTEXT TILL PROGRAM W9032100          
000300*                                                                         
000400     03 RESP-IDAPIORDREF     PIC X(23).                                   
000500*                                 API ORDERID(DIS+KND+ORD+DAT)            
000600     03 RESP-W90321O1-GRP REDEFINES RESP-IDAPIORDREF.                     
000700        05 RESP-IDDISTR-OREF PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900        05 RESP-IDKUNDNR-OREF                                             
001000                             PIC 9(6).                                    
001100*                                 KUNDNUMMER                              
001200        05 RESP-IDORDNR7-OREF                                             
001300                             PIC 9(7).                                    
001400*                                 ORDERNUMMER                             
001500        05 RESP-TIREGDAT-OREF                                             
001600                             PIC 9(6).                                    
001700*                                 REGISTRERINGSDATUM (ÅÅMMDD OR Å         
001800*                                 ÅÅÅ-MM-DD)                              
001900     03 RESP-BEKUNDRF-001    PIC X(10).                                   
002000*                                 KUNDENS REFERENS                        
002100     03 RESP-KDORDKL         PIC 9.                                       
002200*                                 ORDERKLASS                              
002300     03 RESP-IDDISTR         PIC 9(4).                                    
002400*                                 DISTRIKTNUMMER                          
002500     03 RESP-IDKUNDNR        PIC 9(6).                                    
002600*                                 KUNDNUMMER                              
002700     03 RESP-FLCANCEL        PIC X.                                       
002800     03 RESP-TIREPDAT        PIC X(10).                                   
002900*                                 REPAIR DATE                             
003000     03 RESP-TIAAAA-MM-DD REDEFINES RESP-TIREPDAT.                        
003100        05 RESP-TIAAAA       PIC 9(4).                                    
003200*                                 ÅRTAL (ÅÅÅÅ)                            
003300        05 RESP-TEHYPHEN     PIC X.                                       
003400         88 RESP-HYPHEN      VALUE '-'.                                   
003500*                                 BINDESTRECK                             
003600        05 RESP-TIMM         PIC 9(2).                                    
003700*                                 MÅNAD (MM)                              
003800        05 RESP-TEHYPHEN     PIC X.                                       
003900         88 RESP-HYPHEN      VALUE '-'.                                   
004000*                                 BINDESTRECK                             
004100        05 RESP-TIDD         PIC 9(2).                                    
004200*                                 DAG I MÅNAD (DD)                        
004300     03 RESP-W90321O1-001-GRP.                                            
004400        05 RESP-IDNAMN       PIC X(40).                                   
004500*                                 NAMN                                    
004600        05 RESP-IDMAIL       PIC X(60).                                   
004700*                                 MAIL ADRESS                             
004800        05 RESP-BETELNR      PIC X(20).                                   
004900*                                 TELEFONNUMMER                           
005000     03 RESP-W90321O1-002-GRP.                                            
005100        05 RESP-BEGMT-RAD1   PIC X(35).                                   
005200*                                 GODSMOTTAGARNAMN RAD 1                  
005300        05 RESP-BEGMT-RAD2   PIC X(35).                                   
005400*                                 GODSMOTTAGARNAMN RAD 2                  
005500        05 RESP-ADGMT-GATA   PIC X(35).                                   
005600*                                 GODSMOTTAGARADRESS GATA                 
005700        05 RESP-ADCITY       PIC X(20).                                   
005800        05 RESP-ADPOSTNR     PIC X(10).                                   
005900*                                 POSTNUMMER I ADRESS                     
006000        05 RESP-ADGMT-LAND   PIC X(35).                                   
006100*                                 GODSMOTTAGARADRESS LAND                 
006200        05 RESP-IDLANDX2     PIC X(2).                                    
006300*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
006400     03 RESP-BELAGINS-DEL    PIC X(60).                                   
006500*                                 DEL AV LAGERINSTRUKTION                 
006600     03 RESP-KVRADER         PIC 9(3).                                    
006700*                                 ANTAL RADER                             
006800     03 RESP-W90321O1-RAD    OCCURS 1 TO 999 TIMES                        
006900                             DEPENDING ON RESP-KVRADER.                   
007000        05 RESP-IDLEVART     PIC X(30).                                   
007100*                                 LEVERANTÖRENS ARTNR                     
007200        05 RESP-KVBEART      PIC 9(7).                                    
007300*                                 BESTÄLLT ANTAL STYCKEN                  
007400        05 RESP-IDLEVART-DEL PIC X(30).                                   
007500*                                 LEVERANTÖRENS ARTNR                     
007600        05 RESP-KVBEART-Q    PIC 9(7).                                    
007700*                                 BESTÄLLT KVANTANPASSAT ANTAL            
007800        05 RESP-KVLEVART     PIC 9(7).                                    
007900*                                 LEVERERAT ANTAL STYCK                   
008000        05 RESP-IDDC         PIC X(2).                                    
008100*                                 IDENTIFIERARE LAGER                     
008200        05 RESP-BEART        PIC X(25).                                   
008300*                                 ARTIKELBENÄMNING                        
008400        05 RESP-BERADREF     PIC X(10).                                   
008500*                                 KUNDENS RADREFERENS                     
008600        05 RESP-KDSORT       PIC X(2).                                    
008700*                                 SORT-KOD                                
008800        05 RESP-IDFAKT       PIC 9(7).                                    
008900*                                 FAKTURANUMMER                           
009000        05 RESP-IDORDNR7     PIC 9(7).                                    
009100*                                 ORDERNUMMER                             
009200        05 RESP-IDKOLLI      PIC 9(5).                                    
009300*                                 KOLLINUMMER                             
009400        05 RESP-TIDISPIN     PIC X(10).                                   
009500*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
009600        05 RESP-TIRFS        PIC X(10).                                   
009700*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
009800        05 RESP-KDMEDD-PART  PIC 9(3).                                    
009900*                                 MEDDELANDEKOD                           
010000        05 RESP-KDMEDD-QUANT PIC 9(3).                                    
010100*                                 MEDDELANDEKOD                           
010200        05 RESP-KDMEDD-STATUS                                             
010300                             PIC 9(3).                                    
010400*                                 MEDDELANDEKOD                           
010500     03 RESP-IDMFSINF        PIC X(3).                                    
010600*                                 MFS INFO. MEDDELANDE NUMMER             
010700     03 RESP-TEMFSINF        PIC X(55).                                   
010800*                                 INFORMATIONSMEDDELANDE                  
010900*** END OF VILMAII-COPY LENGTH= 168300 BYTES                              
