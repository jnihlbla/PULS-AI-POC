000100 01  SEQB-WDA5B1.                                                         
000200*                                 ORDERRADSREGISTER                       
000300*                                 SEKUNDÄRT INDEX TILL WDA501             
000400*                                 RESERVERADE RADER(KVRESS FINNS)         
000500*                                 EXIT: INDEX FINNS NÄR                   
000600*                                 KDSTARAD  = 3                           
000700*                                 FYSISK NYCKEL: WDA5B1KY                 
000800*                                  (IDDISTR,  IDKUNDNR, IDDC,             
000900*                                   IDARTNR, IDKUNDRF, IDLOPNR )          
001000*                                 SECONDARY NYCKEL: WDA5BSEQ              
001100*                                  (IDDISTR, IDKUNDNR, IDDC)              
001200     03 SEQB-IDDISTR         PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 SEQB-IDKUNDNR        PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800     03 SEQB-IDDC            PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000*                                 WAREHOUSE IDENTIFIER                    
002100     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300*                                 PART NUMBER                             
002400     03 SEQB-IDKUNDRF-GRP.                                                
002500*                                 KUNDENS REFERENS (ORDERID)              
002600*                                 CUSTOMER REFERENCE (ORDER ID)           
002700        05 SEQB-IDKUNDRF     PIC X(10).                                   
002800*                                 KUNDENS REFERENS (ORDERID)              
002900*                                 CUSTOMER REFERENCE (ORDER ID)           
003000        05 SEQB-IDORDNR5-FILLER REDEFINES SEQB-IDKUNDRF.                  
003100           07 SEQB-IDORDNR5  PIC 9(5).                                    
003200*                                 ORDERNUMMER                             
003300*                                 ORDER NUMBER                            
003400           07 FILLER         PIC X(5).                                    
003500        05 SEQB-IDORDNR7-FILLER REDEFINES SEQB-IDKUNDRF.                  
003600           07 SEQB-IDORDNR7  PIC 9(7).                                    
003700*                                 ORDERNUMMER                             
003800*                                 ORDER NUMBER                            
003900           07 FILLER         PIC X(3).                                    
004000     03 SEQB-IDLOPNR         PIC S9(3)           COMP-3.                  
004100*                                 LÖPNUMMER                               
004200*                                 SEQUENCE NUMBER                         
004300     03 SEQB-FLERS           PIC X.                                       
004400*                                 TILLKOMMANDE ARTIKEL ?                  
004500     03 SEQB-IDANSK          PIC S9(3)           COMP-3.                  
004600*                                 ANSKAFFARNUMMER                         
004700*                                 PROCURER NO.                            
004800     03 SEQB-IDANALYS        PIC X(12).                                   
004900*                                 ANALYSNUMMER                            
005000*                                 ANALYSIS NUMBER                         
005100     03 SEQB-IDKONTO         PIC S9(11)          COMP-3.                  
005200*                                 KONTO                                   
005300*                                 ACCOUNT                                 
005400     03 SEQB-IDKST           PIC X(10).                                   
005500*                                 KOSTNADSSTÄLLE                          
005600*                                 COST CENTRE                             
005700     03 SEQB-KDFAKTYP        PIC X.                                       
005800*                                 FAKTURATYP                              
005900*                                 INVOICE TYPE                            
006000     03 SEQB-KDFRAKT         PIC S9(3)           COMP-3.                  
006100*                                 FRAKTSÄTT DC TILL KUND                  
006200*                                 FREIGHT CODE                            
006300     03 SEQB-KDORDKL         PIC S9              COMP-3.                  
006400*                                 ORDERKLASS                              
006500*                                 ORDER CLASS                             
006600     03 SEQB-KDPRODSL        PIC S9(3)           COMP-3.                  
006700*                                 PRODUKTSLAG                             
006800*                                 PRODUCT GROUP                           
006900     03 SEQB-KDRAPRIO        PIC S9(3)           COMP-3.                  
007000*                                 PRIORITETSKOD PÅ RADEN                  
007100*                                 PRIORITY CODE ON THE LINE               
007200     03 SEQB-KDROO           PIC S9              COMP-3.                  
007300*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
007400*                                 REASON CODE FOR WAITING LINE            
007500     03 SEQB-KDSTARAD        PIC X.                                       
007600*                                 RADSTATUSKOD                            
007700*                                 LINE STATUS CODE                        
007800     03 SEQB-KDTPOTYP        PIC S9              COMP-3.                  
007900*                                 TYP AV TIDPLANERAD ORDER                
008000*                                 TYPE OF TIME PLANNED ORDER              
008100     03 SEQB-KVART           PIC S9(7)           COMP-3.                  
008200*                                 ANTAL ARTNR PER BRYTBEGREPP             
008300*                                 NO OF PARTNOS PER TYPE                  
008400     03 SEQB-KDVRINFO        PIC S9              COMP-3.                  
008500*                                 PÅVERKAN I VR/DSP SYSTEM                
008600*                                 VR/DSP UP-DATE                          
008700     03 SEQB-KVRO            PIC S9(7)           COMP-3.                  
008800*                                 ANTAL RESTNOTERADE ARTIKLAR             
008900*                                 BACKORDERED QTY                         
009000     03 SEQB-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
009100*                                 ARTIKELPRIS NETTO                       
009200*                                 NET PRICE EACH   (FOB NET)              
009300     03 SEQB-REKSIFFR        PIC S9              COMP-3.                  
009400*                                 KONTROLLSIFFRA                          
009500*                                 PART NO CHECK DIGIT                     
009600     03 SEQB-TIAVBOKN        PIC S9(7)           COMP-3.                  
009700*                                 LAGERAVBOKNINGSDATUM                    
009800*                                 STOCK ALLOCATION DATE                   
009900     03 SEQB-TIREGDAT        PIC S9(7)           COMP-3.                  
010000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010100*                                 REGISTRATION DATE (YYMMDD)              
010200     03 SEQB-TIRES           PIC S9(7)           COMP-3.                  
010300*                                 RESERVATIONSDATUM                       
010400*                                 RESERVATION DATE                        
010500     03 SEQB-DARODAT         PIC 9(8).                                    
010600*                                 RESTORDERDATUM       (ÅÅÅÅMMDD)         
010700*                                 BACK ORDER DATE      (YYYYMMDD)         
010800     03 SEQB-TITPO           PIC S9(7)           COMP-3.                  
010900*                                 PLANERAD ORDERDATUM                     
011000*                                 PLANNED ORDER DATE                      
011100     03 SEQB-KDPRTYP         PIC X.                                       
011200*                                 TYP AV PRISTILLÄMPNING                  
011300*                                 TYPE OF PRICING                         
011400     03 SEQB-BEVOLREF        PIC X(10).                                   
011500*                                 VOLVO REFERENS                          
011600*                                 VOLVO REFERENCE                         
011700     03 SEQB-FLINVEST        PIC X.                                       
011800*                                 BYTES INVENTERINGSFLAGGA                
011900*                                 EXCHANGE INVESTMENT FLAG                
012000     03 SEQB-FLPRTILL        PIC X.                                       
012100*                                 PRISTILLÄGGS FLAGGA                     
012200*                                 PRICE PENALTY FLAG                      
012300     03 SEQB-FLTPOBEK        PIC X.                                       
012400*                                 TPO-RAD BEKRÄFTAD                       
012500*                                 TPO ORDER LINE CONFIRMED                
012600     03 SEQB-IDWDA501        PIC X(24).                                   
012700*                                 NYCKEL TILL WDA501                      
012800*                                 KEY TO WDA501                           
012900*** END OF VILMAII-COPY LENGTH= 145 BYTES                                 
