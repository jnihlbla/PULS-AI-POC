000100 01  KREG-W411KREG.                                                       
000200*                                 LÄNKAREA TILL W411KREG                  
000300*                                 LÄSNING AV KUNDREGISTRET                
000400*                                                                         
000500     03 KREG-IDDISTR         PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 KREG-IDKUNDNR        PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 KREG-IDDC-TVS        PIC X(2).                                    
001000*                                 DISTRIBUTIONCENTER                      
001100*                                 TVÅNGSSTYRNING                          
001200     03 KREG-IDSYSTEM        PIC X(4).                                    
001300*                                 VOLVO VCCS SYSTEMNUMMER                 
001400     03 KREG-KDFAKTYP-IN     PIC X.                                       
001500*                                 FAKTURATYP                              
001600     03 KREG-KDFRAKT-IN      PIC S9(3)           COMP-3.                  
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800     03 KREG-KDORDKL         PIC S9              COMP-3.                  
001900*                                 ORDERKLASS                              
002000     03 KREG-FLVORKO         PIC X.                                       
002100*                                 VOR-KÖ FLAGGA                           
002200     03 KREG-FLVORFK         PIC X.                                       
002300*                                 VOR-FRAKTKOD FRÅN KLASS 1               
002400     03 KREG-WDB101-DATA.                                                 
002500*                                 WDB101-DATA                             
002600        05 KREG-ADBETRAD-1   PIC X(35).                                   
002700*                                 ADRESSRAD BETALNINGSANSVARIG            
002800        05 KREG-ADBETRAD-2   PIC X(35).                                   
002900*                                 ADRESSRAD BETALNINGSANSVARIG            
003000        05 KREG-BEBETRAD-1   PIC X(35).                                   
003100*                                 DEL AV BETALNINGSANSVARIGS NAMN         
003200        05 KREG-BEBETRAD-2   PIC X(35).                                   
003300*                                 DEL AV BETALNINGSANSVARIGS NAMN         
003400        05 KREG-KDKREDSP     PIC X.                                       
003500*                                 KREDITSPÄRR PÅ BETALARE                 
003600        05 KREG-KDVALISO     PIC X(3).                                    
003700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003800     03 KREG-WDB201-DATA.                                                 
003900*                                 WDB201-DATA                             
004000        05 KREG-ADGMT.                                                    
004100*                                 GODSMOTTAGARADRESS                      
004200           07 KREG-ADGMT-GATA                                             
004300                             PIC X(35).                                   
004400*                                 GODSMOTTAGARADRESS GATA                 
004500           07 KREG-ADGMT-PADR                                             
004600                             PIC X(35).                                   
004700*                                 GODSMOTTAGARADRESS POSTADRESS           
004800           07 KREG-ADPOST-PNRORT REDEFINES KREG-ADGMT-PADR.               
004900*                                 POSTNUMMER + ORT                        
005000              09 KREG-ADPOSTNR                                            
005100                             PIC X(10).                                   
005200*                                 POSTNUMMER I ADRESS                     
005300              09 KREG-ADCITY PIC X(25).                                   
005400*                                 BENÄMNING PÅ STAD                       
005500           07 KREG-ADPOST-ORTPNR REDEFINES KREG-ADGMT-PADR.               
005600*                                 ORT + POSTNUMMER                        
005700              09 KREG-ADCITY PIC X(25).                                   
005800*                                 BENÄMNING PÅ STAD                       
005900              09 KREG-ADPOSTNR                                            
006000                             PIC X(10).                                   
006100*                                 POSTNUMMER I ADRESS                     
006200           07 KREG-ADGMT-LAND                                             
006300                             PIC X(35).                                   
006400*                                 GODSMOTTAGARADRESS LAND                 
006500        05 KREG-BEGMT.                                                    
006600*                                 GODSMOTTAGARNAMN                        
006700           07 KREG-BEGMT-RAD1                                             
006800                             PIC X(35).                                   
006900*                                 GODSMOTTAGARNAMN RAD 1                  
007000           07 KREG-BEGMT-RAD2                                             
007100                             PIC X(35).                                   
007200*                                 GODSMOTTAGARNAMN RAD 2                  
007300        05 KREG-FLAUTORD     PIC X.                                       
007400*                                 FLAGGAN STYR OM AUTOMATORDER-           
007500*                                 NUMMER SKALL SKAPAS                     
007600        05 KREG-FLNC         PIC X.                                       
007700*                                 NEW CONCEPT FLAGGA                      
007800        05 KREG-FLOKFAK-G    PIC X.                                       
007900*                                 FLAGGA FAKTURATYP G GODKÄND             
008000        05 KREG-FLOKFAK-K    PIC X.                                       
008100*                                 FLAGGA FAKTURATYP K GODKÄND             
008200        05 KREG-FLOKFAK-N    PIC X.                                       
008300*                                 FLAGGA FAKTURATYP N GODKÄND             
008400        05 KREG-FLOKFAK-R    PIC X.                                       
008500*                                 FLAGGA FAKTURATYP R GODKÄND             
008600        05 KREG-FLPRELRO     PIC X.                                       
008700*                                 PRELIMINÄR RESTORDERFLAGGA              
008800        05 KREG-FLPRERS      PIC X.                                       
008900*                                 PRISERSÄTTNINGSFLAGGA                   
009000        05 KREG-FLRESTN      PIC X.                                       
009100*                                 RESTNOTERING ?                          
009200        05 KREG-FLORDTIL-KL1 PIC X.                                       
009300*                                 TVINGANDE TILÄGG ORDER KLASS 1          
009400        05 KREG-FLORDTIL-KL2 PIC X.                                       
009500*                                 TVINGANDE TILÄGG ORDER KLASS 2          
009600        05 KREG-FLORDTIL-KL3 PIC X.                                       
009700*                                 TVINGANDE TILÄGG ORDER KLASS 3          
009800        05 KREG-FLORDTIL-KL4 PIC X.                                       
009900*                                 TVINGANDE TILÄGG ORDER KLASS 4          
010000        05 KREG-FILLER       PIC X(8).                                    
010100        05 KREG-IDDEPOT      PIC X(2).                                    
010200*                                 TRANSPORT DEPOT                         
010300        05 KREG-IDRFTAB      PIC X(3).                                    
010400*                                 RANSONERINGSFAKTORTABELL                
010500        05 KREG-IDROUTE      PIC X.                                       
010600*                                 TRANSPORT ROUTE                         
010700        05 KREG-IDSKYLT      PIC X(3).                                    
010800*                                 NATIONALITETSTECKEN                     
010900*                                 SPRÅKIDENTIFIKATION                     
011000        05 KREG-IDZON        PIC X(2).                                    
011100*                                 TRANSPORTVÄG (RUTT,ZON)                 
011200        05 KREG-KDBEKALT     PIC S9              COMP-3.                  
011300*                                 ORDERBEKRÄFTELSEALTERNATIV              
011400        05 KREG-KDGENFAK     PIC X.                                       
011500*                                 NORMAL FAKTURATYP                       
011600        05 KREG-KDORDING     PIC S9              COMP-3.                  
011700*                                 UPPDATERING ORDERINGÅNG                 
011800        05 KREG-KVDAGAR-DOW  PIC S9(3)           COMP-3.                  
011900*                                 ANTAL DAGAR FÖRE DC CLEARING            
012000        05 KREG-REAVDRAG     PIC S9(2)V9(1)      COMP-3.                  
012100*                                 AVDRAGSPROCENT                          
012200        05 KREG-REEMBHNT     PIC S9(2)V9(1)      COMP-3.                  
012300*                                 EMB OCH HANTERINGSKOST (%)              
012400        05 KREG-RESLATT      PIC S9(3)           COMP-3.                  
012500*                                 SLATTGRÄNS                              
012600     03 KREG-WDB301-DATA.                                                 
012700*                                 WDB301-DATA                             
012800        05 KREG-IDDC         PIC X(2).                                    
012900*                                 IDENTIFIERARE LAGER                     
013000        05 KREG-KDMOMSIN     PIC S9              COMP-3.                  
013100*                                 MOMSINSTRUKTION                         
013200        05 KREG-KDROPACK     PIC X.                                       
013300*                                 FRISLÄPPNINGSKOD RO/DO                  
013400        05 KREG-KDTULLVE     PIC S9              COMP-3.                  
013500*                                 TYP AV PRIS PÅ TULLFAKTURA              
013600        05 KREG-KVLEDTIM-0   PIC S9(3)V9(2)      COMP-3.                  
013700*                                 LEDTID KL 0                             
013800        05 KREG-KVLEDTIM-1   PIC S9(3)V9(2)      COMP-3.                  
013900*                                 LEDTID KL 1                             
014000        05 KREG-KVLEDTIM-2   PIC S9(3)V9(2)      COMP-3.                  
014100*                                 LEDTID KL 2                             
014200        05 KREG-KVLEDTIM-3   PIC S9(3)V9(2)      COMP-3.                  
014300*                                 LEDTID KL 3                             
014400        05 KREG-KVLEDTIM-4   PIC S9(3)V9(2)      COMP-3.                  
014500*                                 LEDTID KL 4                             
014600     03 KREG-WDB501-DATA.                                                 
014700*                                 WDB501-DATA                             
014800        05 KREG-KDFRAKT      PIC S9(3)           COMP-3.                  
014900*                                 FRAKTSÄTT DC TILL KUND                  
015000        05 KREG-BEGMRK.                                                   
015100*                                 GODSMÄRKE                               
015200           07 KREG-BEGMRK-RAD1                                            
015300                             PIC X(30).                                   
015400*                                 GODSMÄRKE  RAD1                         
015500           07 KREG-BEGMRK-RAD2                                            
015600                             PIC X(30).                                   
015700*                                 GODSMÄRKE  RAD2                         
015800        05 KREG-IDTRP.                                                    
015900*                                 TRANSPORTIDENTITET                      
016000           07 KREG-IDTRPLOS  PIC X(3).                                    
016100*                                 TRANSPORTLÖSNING                        
016200           07 KREG-IDTRPVAR  PIC X(2).                                    
016300*                                 TRANSPORTLÖSNINGSGRUPP                  
016400        05 KREG-IDTRP-ALT.                                                
016500*                                 TRANSPORT-ID ALTERNATIV                 
016600           07 KREG-IDTRPLOS-ALT                                           
016700                             PIC X(3).                                    
016800*                                 TRANSPORTLÖSNING                        
016900           07 KREG-IDTRPVAR-ALT                                           
017000                             PIC X(2).                                    
017100*                                 TRANSPORTLÖSNINGSGRUPP                  
017200        05 KREG-KDFDKRAV     PIC S9(3)           COMP-3.                  
017300*                                 TRANSPORTFÖRPACKNINGSKOD                
017400        05 KREG-KDTRPKAT     PIC X.                                       
017500*                                 TRANSPORTKATEGORI                       
017600        05 KREG-PRLEGKST     PIC S9(7)V9(2)      COMP-3.                  
017700*                                 LEGALISERINSKOSTNAD                     
017800        05 KREG-REFOERS      PIC S9(2)V9(3)      COMP-3.                  
017900*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
018000     03 KREG-KREG-FEL.                                                    
018100*                                 KREG-FEL                                
018200        05 KREG-IDDISTR-OK   PIC X.                                       
018300        05 KREG-IDKUNDNR-OK  PIC X.                                       
018400        05 KREG-IDDC-OK      PIC X.                                       
018500        05 KREG-KDFRAKT-OK   PIC X.                                       
018600        05 KREG-IDVAT-OK     PIC X.                                       
018700        05 KREG-IDPARTNR-OK  PIC X.                                       
018800*** END OF VILMAII-COPY LENGTH= 490 BYTES                                 
