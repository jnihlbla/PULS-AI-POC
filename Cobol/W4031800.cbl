000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0123      *        
000400******************************************************************        
000500*                                                                         
      *COMPOPT VPOSIX=YES                                                       
000600 ID DIVISION.                                                             
000700     SKIP2                                                                
000800 PROGRAM-ID.     W4031800.                                                
000900 AUTHOR.         CAP GEMINI AB/EP.                                        
001000     DATE-WRITTEN.   APRIL 86.                                            
001100                                                                          
001200     REMARKS.                                                             
001300                                                                          
001400*    FUNKTION.                                                            
001500*        PROGRAMMET KONTROLLERAR ATT DE TILL PACKAREN UTDELADE            
001600*        ORDERRADERNA ÄR FÄRDIGBEHANDLADE.                                
001700*        OM GODKÄND STARTAS BAKGRUNDSTRANS W40397 SOM UPPDATERAR          
001800*        ORDER- OCH ARTIKELREGISTREN. DESSUTOM SKAPAS EN TRANS-           
001900*        AKTION FÖR AVVIKELSERAD FÖR TRANSAKTIONER TILL ÖVRIGA            
002000*        SYSTEM OCH LÅSNINGSREG SLÅS AV.                                  
002100*        AUTOMATFAKTURATRANS SKAPAS I VISSA FALL.                         
002200*                                                                         
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T318                                              
002600*        MID:         W4I31801                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O31801                                            
003000*        TRANS:       W4T397X                                             
003100*        SE&O                                                             
003200*                                                                         
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP3                                                                
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77   PROGRAM-NAMN           VALUE 'W4031800'                             
004100                                 PIC X(8).                                
004200 77    JA                        PIC X       VALUE 'J'.                   
004300 77    YES                       PIC X       VALUE 'Y'.                   
004400 77    NEJ                       PIC X       VALUE 'N'.                   
004500 77    RAETT                     PIC X       VALUE 'R'.                   
004600 77    FEL                       PIC X       VALUE 'F'.                   
004700 77    SOEK-VIA-PRODNR           PIC X       VALUE 'N'.                   
004800 77    WS-WDE401-FEL             PIC X.                                   
004900 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77    INX                       PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77    RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.        
005200 77    BILD-RAD                  PIC S9(9)   VALUE +0   COMP SYNC.        
005300 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +600 COMP SYNC.        
005400 77    4314-MOD-LAENGD           PIC S9(4)   VALUE +419 COMP SYNC.        
005500 77    4315-MOD-LAENGD           PIC S9(4)   VALUE +400 COMP SYNC.        
005600 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
005700 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
005800 77    WS-IDDISTR                PIC X(4)   VALUE SPACE.                  
005900 77    WS-IDDISTR-NUM            PIC 9(4)   VALUE ZERO.                   
006000 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
006100 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
006200 77    WS-IDKOLLI                PIC X(5)   VALUE SPACE.                  
006300 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
006400 77    WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                  
006500 77    WS-IDRADNR-25             PIC X(4)   VALUE ZERO.                   
006600 77    WS-IDPLKLST               PIC 9(3)   VALUE ZERO.                   
006700 77    WS-IDPLKLST-SPAR          PIC 9(3)   VALUE ZERO.                   
006800 77    WS-IDPLKLST-NAESTA        PIC 9(3)   VALUE ZERO.                   
006900 77    WS-IDPLKLST-SPAR-AR-HOGST   PIC X.                                 
007000 77    WS-IDPLKLST-NAESTA-AR-HOGST PIC X.                                 
007100 77    WS-IDPLKLST-FOM           PIC 9(3)   VALUE ZERO.                   
007200 77    WS-JFR-IDPLKLST           PIC 9(3)   VALUE ZERO.                   
007300 77    WS-IDRADNR-SPAR           PIC 9(4)   VALUE ZERO.                   
007400 77    WS-KVORDRAD-LEVPL-SPAR    PIC S9(5) COMP-3 VALUE ZERO.             
007500 77    WS-IDRADNR-RED            PIC Z(4).                                
007600 77    WS-FLPAFEL                PIC X(1)   VALUE SPACE.                  
007700 77    WS-FLNOLLJ                PIC X(1)   VALUE SPACE.                  
007800 77    WS-KVORAPP                PIC 9(6)   VALUE ZERO.                   
007900 77    WS-KVORAPP-RED            PIC Z(6).                                
008000 77    WS-KDORDKL                PIC 9(1)   VALUE ZERO.                   
008100 77    WS-ANT-ODEL-KVAR-ATT-BEHANDLA PIC 9(3)   VALUE ZERO.               
008200     EJECT                                                                
008300 01    DYNAMISKA-SUBPROGRAM.                                              
008400   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
008500   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
         03  W488ORCN                  PIC X(8)    VALUE 'W488ORCN'.            
008600   03  W005INIT                  PIC X(8)   VALUE 'W005INIT'.             
008700   03  W006PRT                   PIC X(8)   VALUE 'W006PRT '.             
008800     SKIP2                                                                
008900*    --- LÄNK-AREOR TILL GEMENSAMMA SUBPROGRAM                            
009000 01  FILLER                      PIC X(16)  VALUE 'WMSGINIT '.            
009100*01 -COPY WMSGINIT                                                        
009200 01  FILLER                      PIC X(16)  VALUE 'W006PRT  '.            
009300*   -COPY W006PRT                                                         
009400     EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'W488ORCN'.            
      *01 -COPY W488ORCN                                                        
           EJECT                                                                
009500 77    MAX-ANTAL-RADER           PIC S9(3)  VALUE +25  COMP-3.            
009600 77    MAX-ANT-RADER-PLUS-1      PIC S9(3)  VALUE +26  COMP-3.            
009700 77    WS-TRAEFF-PACKARE         PIC X(01).                               
009800   88  TRAEFF-PACKARE                       VALUE 'J'.                    
009900 77    WS-FLER-ORDERDELAR-FINNS  PIC X(01).                               
010000   88  FLER-ORDERDELAR-FINNS                VALUE 'J'.                    
010100 77    WS-SLINGA-KLAR            PIC X(01).                               
010200   88  SLINGA-KLAR                          VALUE 'J'.                    
010300 77    WS-KOLLIFEL               PIC X(01).                               
010400   88  KOLLIFEL                             VALUE 'J'.                    
010500 77    WS-IDTRANS                PIC X(04).                               
010600   88  WS-SAMMA-BILD                        VALUE '4318'.                 
010700   88  WS-GODKAND-BILD                      VALUE '4311' '4312'           
010800                                                  '4314' '4315'           
010900                                                  '4316' '4317'           
011000                                                  '4318' '4325'.          
011100     SKIP2                                                                
011200 77    WS-INDATA-TEST            PIC X(01).                               
011300   88  WS-INDATA-FEL                        VALUE 'F'.                    
011400   88  WS-INDATA-RATT                       VALUE 'R'.                    
011500     SKIP2                                                                
011600 77    WS-BEHANDLING-TEST        PIC X(01).                               
011700   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
011800   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
011900     SKIP2                                                                
012000 77    WS-4397-STARTAD-SW        PIC X(01).                               
012100   88  WS-4397-STARTAD                      VALUE 'J'.                    
012200     SKIP2                                                                
012300 77    WS-PACKN-OMRADE           PIC 9(02).                               
012400   88   WS-NOLLNING-PACK-OMR                 VALUE  14 40 41              
012500                                              43 44 45 48 49              
012600                                              57 59 70 71 72              
012700                                              75 76 85 90.                
012800*                                                                         
012900*      --- VALID IDDD CODES                                               
013000*                                                                         
013100*01    -COPY WWDC99                                                       
013200       EJECT                                                              
013300 01  WS-IDPRTLST.                                                         
013400     03 WS-SYSTDEL               PIC X(1).                                
013500     03 WS-LISTTYP               PIC X(2).                                
013600     03 WS-DC                    PIC X(2).                                
013700     03 WS-KDPRT                 PIC X(3).                                
013800*                                                                         
013900 01    TESTMED.                                                           
014000   03  TESTMED1                 PIC 9(3).                                 
014100   03  FILLER                   PIC X(2).                                 
014200   03  TESTMED2                 PIC 9(5).                                 
014300   03  FILLER                   PIC X(2).                                 
014400   03  TESTMED3                 PIC X(1).                                 
014500   03  FILLER                   PIC X(27).                                
014600 01    WS-IDRADNR-X                                 PIC X(4).             
014700 01    WS-IDRADNR-NUM   REDEFINES WS-IDRADNR-X      PIC 9(4).             
014800     SKIP2                                                                
014900 01    WS-ADLAGOMR-N             PIC 9(3).                                
015000 01    WS-ADLAGOMR-X REDEFINES WS-ADLAGOMR-N.                             
015100   03  FILLER                    PIC X.                                   
015200   03  WS-ADLAGOMR               PIC X(2).                                
015300     SKIP2                                                                
015400 01    WS-IDKUNDRF.                                                       
015500   03  WS-IDORDNR                PIC X(5).                                
015600   03  FILLER                    PIC X(5)   VALUE SPACE.                  
015700 01    WS-JFR-IDANSTNR.                                                   
015800   03  FILLER                    PIC X(3).                                
015900   03  WS-JFR-IDANSTNR-5         PIC X(5).                                
016000     SKIP2                                                                
016100 77    FL-531-SEGMENT            PIC X(01).                               
016200   88  ORAPP-RADER-FINNS                    VALUE 'J'.                    
016300   88  ORAPP-RADER-SAKNAS                   VALUE 'N'.                    
016400     SKIP2                                                                
016500 77    FL-NYCKLAR                PIC X(01).                               
016600   88  FL-NYA-NYCKLAR                       VALUE 'J'.                    
016700   88  FL-GAMLA-NYCKLAR                     VALUE 'N'.                    
016800                                                                          
016900 01   ARB-ADRESS.                                                         
017000    05 ARB-ADFLGEO               PIC X(3)   VALUE SPACE.                  
017100    05 FILLER                    PIC X      VALUE SPACE.                  
017200    05 ARB-ADFLOMR               PIC 9(3)   VALUE ZERO.                   
017300    05 FILLER                    PIC X      VALUE SPACE.                  
017400    05 ARB-ADRUTNIV              PIC 9(3)   VALUE ZERO.                   
017500     SKIP2                                                                
017600     EJECT                                                                
017700 01    NYCKLAR-TILL-DLI.                                                  
017800*                                                                         
017900   03    W-WDE4A1-KUNDORDER-X.                                            
018000     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
018100     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
018200     05    W-4A1-IDKUNDRF.                                                
018300       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
018400       07  FILLER                PIC X(05)   VALUE SPACE.                 
018500*                                                                         
018600   03    W-WDE401-KUNDORDER-X.                                            
018700     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
018800     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
018900     05    W-401-IDKUNDRF.                                                
019000       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
019100       07  FILLER                PIC X(05)   VALUE SPACE.                 
019200     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
019300     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
019400*                                                                         
019500   03    W-WDE4B-KEYSEQ-MIN-X.                                            
019600     05    W-411-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
019700     05    W-411-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
019800*                                                                         
019900   03    W-WDE4B-KEYSEQ-MAX-X.                                            
020000     05    W-411-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
020100     05    W-411-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
020200*                                                                         
020300   03    W-WDE411-IDPURAD-X.                                              
020400     05    W-411-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
020500*                                                                         
020600   03    W-WDE601-IDPRODNR-X.                                             
020700     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
020800*                                                                         
020900   03    W-4301-WDGXKEY-X.                                                
021000     05    W-4301-IDHTYP         PIC X(4)    VALUE '4301'.                
021100     05    W-4301-IDPRODNR       PIC S9(7)   VALUE ZERO  COMP-3.          
021200     05    W-4301-NYCKEL-VALFRI  PIC X(22)   VALUE LOW-VALUE.             
021300*                                                                         
021400   03    W-4302-WDGXKEY-X.                                                
021500     05    W-4302-IDKOLLI        PIC S9(5)   VALUE ZERO  COMP-3.          
021600     05    W-4302-IDPLKLST       PIC S9(3)   VALUE ZERO  COMP-3.          
021700*                                                                         
021800   03    W-IDDC-B6-X.                                                     
021900     05    W-IDDC-B6             PIC X(2).                                
022000                                                                          
022100     EJECT                                                                
022200 01  W-RAETT-1.                                                           
022300     03 RAETT-1-SVE              PIC X(21)                                
022400        VALUE 'ORDERDELEN AVSLUTAD  '.                                    
022500     03 RAETT-1-ENG              PIC X(21)                                
022600        VALUE 'ORDER PART READY     '.                                    
022700 01  FILLER REDEFINES W-RAETT-1.                                          
022800     03 RAETT-1  OCCURS 2        PIC X(21).                               
022900     SKIP3                                                                
023000 01  W-RAETT-2.                                                           
023100     03 RAETT-2-SVE              PIC X(30)                                
023200        VALUE 'ORDERDELEN EJ AVSLUTAD        '.                           
023300     03 RAETT-2-ENG              PIC X(30)                                
023400        VALUE 'ORDER PART NOT ENDED          '.                           
023500 01  FILLER REDEFINES W-RAETT-2.                                          
023600     03 RAETT-2  OCCURS 2        PIC X(24).                               
023700     SKIP3                                                                
023800 01    MEDDELANDE.                                                        
023900     SKIP2                                                                
024000   03   UPPLYSN-2.                                                        
024100     05 FILLER                   PIC X(61)  VALUE                         
024200        'MER FINNS, OM DU VILL AVBRYTA KONTROLL SVARA N'.                 
024300     05 FILLER                   PIC X(61)  VALUE                         
024400        'MORE LINES, INTERRUPT WITH N      '.                             
024500   03    FILLER REDEFINES UPPLYSN-2.                                      
024600     05  UPPL-2        OCCURS 2   PIC X(61).                              
024700     SKIP2                                                                
024800   03   UPPLYSN-3.                                                        
024900     05 FILLER                   PIC X(61)  VALUE                         
025000        'SISTA SIDAN. SVARA J FÖR OK, ELLER N FÖR AVBRYT'.                
025100     05 FILLER                   PIC X(61)  VALUE                         
025200        'LAST PAGE. ENTER Y FOR OK, OR N TO INTERRUPT'.                   
025300   03    FILLER REDEFINES UPPLYSN-3.                                      
025400     05  UPPL-3        OCCURS 2   PIC X(61).                              
025500     SKIP2                                                                
025600   03   UPPLYSN-4.                                                        
025700     05 FILLER                   PIC X(61)  VALUE                         
025800        'FLER ORDERDELAR FINNS                         '.                 
025900     05 FILLER                   PIC X(61)  VALUE                         
026000        'MORE ORDER PARTS EXIST                        '.                 
026100   03    FILLER REDEFINES UPPLYSN-4.                                      
026200     05  UPPL-4        OCCURS 2   PIC X(61).                              
026300     SKIP2                                                                
026400   03   UPPLYSN-5.                                                        
026500     05 FILLER                   PIC X(61)  VALUE                         
026600        'SISTA ORDERDELEN. SVARA J FÖR OK, ELLER N FÖR AVBRYT'.           
026700     05 FILLER                   PIC X(61)  VALUE                         
026800        'LAST ORDER PART. ENTER Y FOR OK, OR N TO INTERRUPT'.             
026900   03    FILLER REDEFINES UPPLYSN-5.                                      
027000     05  UPPL-5        OCCURS 2   PIC X(61).                              
027100     SKIP2                                                                
027200   03   UPPLYSN-6.                                                        
027300     05 FILLER                   PIC X(61)  VALUE                         
027400        'SISTA ORDERDELEN, FLER EJ AVSLUTADE ORDERDELAR FINNS'.           
027500     05 FILLER                   PIC X(61)  VALUE                         
027600        'LAST ORDER PART. NO MORE FINISHED ORDER PARTS.'.                 
027700   03    FILLER REDEFINES UPPLYSN-6.                                      
027800     05  UPPL-6        OCCURS 2   PIC X(61).                              
027900     SKIP2                                                                
028000   03    FEL-1.                                                           
028100     05  FILLER                  PIC X(40)   VALUE                        
028200        '701 ORDERN SAKNAS                       '.                       
028300     05  FILLER                  PIC X(40)   VALUE                        
028400        '701 ORDER MISSING                       '.                       
028500   03    FILLER  REDEFINES  FEL-1.                                        
028600     05  FEL-701     OCCURS 2    PIC X(40).                               
028700*                                                                         
028800   03    FEL-2.                                                           
028900     05  FILLER                  PIC X(40)   VALUE                        
029000        '716 ORDERN EJ DELAD                     '.                       
029100     05  FILLER                  PIC X(40)   VALUE                        
029200        '716 ORDER HAS NOT BEEN SPLIT.           '.                       
029300   03    FILLER  REDEFINES  FEL-2.                                        
029400     05  FEL-716     OCCURS 2    PIC X(40).                               
029500*                                                                         
029600   03    FEL-3.                                                           
029700     05  FILLER                  PIC X(40)   VALUE                        
029800        '710 ORDERN FÄRDIGRAPPORTERAD            '.                       
029900     05  FILLER                  PIC X(40)   VALUE                        
030000        '710 ORDER TOTALLY REPORTED.             '.                       
030100   03    FILLER  REDEFINES  FEL-3.                                        
030200     05  FEL-718     OCCURS 2    PIC X(40).                               
030300*                                                                         
030400   03    FEL-4.                                                           
030500     05  FILLER                  PIC X(40)   VALUE                        
030600        '719 ANGIVEN PACKARE SAKNAS PÅ ORDERN    '.                       
030700     05  FILLER                  PIC X(40)   VALUE                        
030800        '719 WRONG PACKER                        '.                       
030900   03    FILLER  REDEFINES  FEL-4.                                        
031000     05  FEL-719     OCCURS 2    PIC X(40).                               
031100*                                                                         
031200   03    FEL-5.                                                           
031300     05  FILLER                  PIC X(40)   VALUE                        
031400        '720 ANGIVEN PACKARES ORDERDEL REDAN KLAR'.                       
031500     05  FILLER                  PIC X(40)   VALUE                        
031600        '720 ORDER PART OF PACKER READY.         '.                       
031700   03    FILLER  REDEFINES  FEL-5.                                        
031800     05  FEL-720     OCCURS 2    PIC X(40).                               
031900*                                                                         
032000   03    FEL-6.                                                           
032100     05  FILLER                  PIC X(40)   VALUE                        
032200        '804 AVVIKELSEUPPDATERING PÅGÅR          '.                       
032300     05  FILLER                  PIC X(40)   VALUE                        
032400        '804 DEVIATION CONTROL IN PROGRESS       '.                       
032500   03    FILLER  REDEFINES  FEL-6.                                        
032600     05  FEL-804     OCCURS 2    PIC X(40).                               
032700*                                                                         
032800   03    FEL-7.                                                           
032900     05  FILLER                  PIC X(40)   VALUE                        
033000        '8171 KONTROLLPROBLEM, SE ANVÄNDARHANDBOK'.                       
033100     05  FILLER                  PIC X(40)   VALUE                        
033200        '8171 CONTROL PROBLEMS, SEE MANUAL   '.                           
033300   03    FILLER  REDEFINES  FEL-7.                                        
033400     05  FEL-8171    OCCURS 2    PIC X(40).                               
033500*                                                                         
033600   03    FEL-7B.                                                          
033700     05  FILLER                  PIC X(40)   VALUE                        
033800        '8172 KONTROLLPROBLEM, SE ANVÄNDARHANDBOK'.                       
033900     05  FILLER                  PIC X(40)   VALUE                        
034000        '8172 CONTROL PROBLEMS, SEE MANUAL      '.                        
034100   03    FILLER  REDEFINES  FEL-7B.                                       
034200     05  FEL-8172    OCCURS 2    PIC X(40).                               
034300*                                                                         
034400   03    FEL-8.                                                           
034500     05  FILLER                  PIC X(40)   VALUE                        
034600        '816 RADER/KOLLIN KVAR ATT RAPPORTERA    '.                       
034700     05  FILLER                  PIC X(40)   VALUE                        
034800        '816 LINES/CASES LEFT TO REPORT          '.                       
034900   03    FILLER  REDEFINES  FEL-8.                                        
035000     05  FEL-816     OCCURS 2    PIC X(40).                               
035100*                                                                         
035200   03    FEL-9.                                                           
035300     05  FILLER                  PIC X(40)   VALUE                        
035400        'BLÄDDRA FÄRDIGT ELLER AVBRYT KOLL MED N '.                       
035500     05  FILLER                  PIC X(40)   VALUE                        
035600        'CONTINUE SCROLLING OR INTERRUPT WITH N  '.                       
035700   03    FILLER  REDEFINES  FEL-9.                                        
035800     05  FEL-819     OCCURS 2    PIC X(40).                               
035900*                                                                         
036000   03    FEL-10.                                                          
036100     05  FILLER                  PIC X(40)   VALUE                        
036200        '748 UPPLYSTA FÄLT FELAKTIGA             '.                       
036300     05  FILLER                  PIC X(40)   VALUE                        
036400        '748 HIGHLIT FIELDS WRONG                '.                       
036500   03    FILLER  REDEFINES  FEL-10.                                       
036600     05  FEL-748     OCCURS 2    PIC X(40).                               
036700     SKIP2                                                                
036800   03    FEL-11.                                                          
036900     05  FILLER                  PIC X(40)   VALUE                        
037000        '787 EJ NYA NYCKLAR OCH INMATNING        '.                       
037100     05  FILLER                  PIC X(40)   VALUE                        
037200        '787 NEW KEYS AND INPUT NOT ALLOWED      '.                       
037300   03    FILLER  REDEFINES  FEL-11.                                       
037400     05  FEL-787     OCCURS 2    PIC X(40).                               
037500     SKIP2                                                                
037600   03    FEL-12.                                                          
037700     05  FILLER                  PIC X(40)   VALUE                        
037800        '778 FLER RADER FINNS                    '.                       
037900     05  FILLER                  PIC X(40)   VALUE                        
038000        '778 MORE LINES                          '.                       
038100   03    FILLER  REDEFINES  FEL-12.                                       
038200     05  FEL-778     OCCURS 2    PIC X(40).                               
038300     SKIP2                                                                
038400   03    FEL-13.                                                          
038500     05  FILLER                  PIC X(40)   VALUE                        
038600        '749 FEL NYCKEL                          '.                       
038700     05  FILLER                  PIC X(40)   VALUE                        
038800        '749 WRONG KEY                           '.                       
038900   03    FILLER  REDEFINES  FEL-13.                                       
039000     05  FEL-749     OCCURS 2    PIC X(40).                               
039100     SKIP2                                                                
039200   03    FEL-14.                                                          
039300     05  FILLER                  PIC X(40)   VALUE                        
039400        '833 UPPLYSTA RADER KAN DU EJ GODKÄNNA   '.                       
039500     05  FILLER                  PIC X(40)   VALUE                        
039600        '833 HIGHLIGHT FIELDS CANNOT BE APPROVED '.                       
039700   03    FILLER  REDEFINES  FEL-14.                                       
039800     05  FEL-833     OCCURS 2    PIC X(40).                               
039900     SKIP2                                                                
040000   03    FEL-15.                                                          
040100     05  FILLER                  PIC X(40)   VALUE                        
040200        '187 OPACKADE KOLLIN FINNS               '.                       
040300     05  FILLER                  PIC X(40)   VALUE                        
040400        '187 UNPACKED CASES EXIST                '.                       
040500   03    FILLER  REDEFINES  FEL-15.                                       
040600     05  FEL-187     OCCURS 2    PIC X(40).                               
040700*                                                                         
040800   03    FEL-16.                                                          
040900     05  FILLER                  PIC X(40)   VALUE                        
041000        '749 FEL SKRIVARE                        '.                       
041100     05  FILLER                  PIC X(40)   VALUE                        
041200        '749 WRONG PRINTER                       '.                       
041300   03    FILLER  REDEFINES  FEL-16.                                       
041400     05  FEL-772     OCCURS 2    PIC X(40).                               
041500     SKIP2                                                                
041600     EJECT                                                                
041700******************************************************************        
041800*                                                                *        
041900*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
042000*                                                                *        
042100******************************************************************        
042200 01    FILLER                 PIC X(16) VALUE 'MID W4I31801 MID'.         
042300     SKIP3                                                                
042400*01    MID -COPY W4I31801.                                                
042500     EJECT                                                                
042600*01    -COPY WMSGAREA                                                     
042700     EJECT                                                                
042800*  03    MOD -COPY W4O31801  -RED MSG-AREA.                               
042900     EJECT                                                                
043000*01      MOD -COPY W4O31401  -PRE 4314-.                                  
043100     EJECT                                                                
043200*01      MOD -COPY W4O31501  -PRE 4315-.                                  
043300     EJECT                                                                
043400 01    FILLER                    PIC X(16)   VALUE 'TRANS-AREA '.         
043500 01    4397-TRANSAREA.                                                    
043600   03    4397-IDPRODNR           PIC 9(7)    VALUE ZERO.                  
043700   03    4397-IDANSTNR           PIC 9(5)    VALUE ZERO.                  
043800   03    4397-IDPLKLST           PIC 9(3)    VALUE ZERO.                  
043900   03    4397-IDPURAD            PIC 9(5)    VALUE ZERO.                  
044000   03    FILLER                  PIC X(80)   VALUE SPACE.                 
044100     EJECT                                                                
044200 01    FILLER                    PIC X(16)   VALUE 'ALT-IO-AREA'.         
044300 01    ALT-IO-AREA.                                                       
044400   03    ALT-LL                  PIC S9(4)   COMP  SYNC.                  
044500   03    ALT-Z1                  PIC X(1).                                
044600   03    ALT-Z2                  PIC X(1).                                
044700   03    ALT-TRANSKOD            PIC X(8)    VALUE SPACE.                 
044800   03    ALT-IDTRANS             PIC X(4)    VALUE SPACE.                 
044900   03    ALT-KDMFSFOR            PIC X(1)    VALUE SPACE.                 
045000   03    ALT-AREA                PIC X(105)  VALUE SPACE.                 
045100     SKIP2                                                                
045200 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
045300     SKIP3                                                                
045400*01    -COPY WMFSAREA                                                     
045500     EJECT                                                                
045600******************************************************************        
045700*                                                                         
045800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
045900*                                                                         
046000 01    IMS-WS.                                                            
046100   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
046200     SKIP3                                                                
046300*                        **** STATUS-KOD FRÅN IMS                         
046400   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
046500     88    KUNDORDER-SEK-FINNS               VALUE '  '.                  
046600     88    KUNDORDER-SEK-SAKNAS              VALUE 'GE' 'GB'.             
046700   03    STATUS-ORAD-WS          PIC XX.                                  
046800     88    ORAD-FINNS                        VALUE '  '.                  
046900     88    ORAD-SAKNAS                       VALUE 'GE'.                  
047000   03    STATUS-WS               PIC XX.                                  
047100     88    SEGMENT-FINNS                     VALUE '  '.                  
047200     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
047300     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
047400     SKIP3                                                                
047500   03    GODK-STATUSKODER.                                                
047600     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
047700     SKIP3                                                                
047800 01    SSA1                      PIC X(64).                               
047900 01    SSA2                      PIC X(64).                               
048000 01    SSA3                      PIC X(64).                               
048100 01    SSA4                      PIC X(64).                               
048200     EJECT                                                                
048300*                            IMS FUNKTIONSKODER                           
048400*01    -COPY W0003                                                        
048500     EJECT                                                                
048600*                            DLI INPUT-OUTPUT AREA                        
048700 01    DLI-IO-AREA.                                                       
048800   03    IO-AREA                 PIC X(550)  VALUE SPACE.                 
048900     SKIP3                                                                
049000*  03    WDE401 -COPY WDE401               -RED IO-AREA.                  
049100     EJECT                                                                
049200*  03    WDE411 -COPY WDE411               -RED IO-AREA.                  
049300     EJECT                                                                
049400*  03    WDE601 -COPY WDE601               -RED IO-AREA.                  
049500     EJECT                                                                
049600 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA1'.          
049700 01    DLI-IO-AREA1.                                                      
049800   03    IO-AREA1                PIC X(100)  VALUE SPACE.                 
049900*  03    WLXXDU01  -COPY WDGX4301    -RED IO-AREA1.                       
050000     EJECT                                                                
050100*  03    WLXXDU11  -COPY WDGX4302    -RED IO-AREA1.                       
050200                                                                          
050300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
050400 01   DLI-IO-AREA-B601.                                                   
050500*     03  -COPY WDB601                                                    
050600     EJECT                                                                
050700 LINKAGE SECTION.                                                         
050800*01    -COPY W0009     -PRE MSG-                                          
050900     EJECT                                                                
051000*01    -COPY W0009     -PRE ALT-                                          
051100     EJECT                                                                
051000*01    -COPY W0009     -PRE SYNQ-                                         
051100     EJECT                                                                
051200*01    -COPY W0008     -PRE WDP7-                                         
051300     05  FILLER                  PIC X.                                   
051400     EJECT                                                                
051500*01    -COPY W0008     -PRE WDE4-                                         
051600     05  FILLER                  PIC X.                                   
051700     EJECT                                                                
051800*01    -COPY W0008     -PRE WDE42-                                        
051900     05  FILLER                  PIC X.                                   
052000     EJECT                                                                
052100*01    -COPY W0008     -PRE WDE43-                                        
052200     05  FILLER                  PIC X.                                   
052300     EJECT                                                                
052400*01    -COPY W0008     -PRE WDE6-                                         
052500     05  FILLER                  PIC X.                                   
052600     EJECT                                                                
052700*01    -COPY W0008     -PRE XXDU-                                         
052800     05  FILLER                  PIC X.                                   
052900     EJECT                                                                
053000*01    -COPY W0008     -PRE WDB6-                                         
053100     05  FILLER                  PIC X.                                   
053200     EJECT                                                                
       01  SYNQ-ATAB-PCB             PIC X.                                     
       01  WDQ3-PCB                  PIC X.                                     
053300 PROCEDURE DIVISION USING  MSG-PCB   ALT-PCB SYNQ-PCB WDP7-PCB            
053400                           WDE4-PCB  WDE42-PCB                            
053500                           WDE43-PCB WDE6-PCB                             
053600                           XXDU-PCB  WDB6-PCB                             
                                 SYNQ-ATAB-PCB WDQ3-PCB.                        
053700     ENTRY 'DLITCBL' USING MSG-PCB   ALT-PCB SYNQ-PCB WDP7-PCB            
053800                           WDE4-PCB  WDE42-PCB                            
053900                           WDE43-PCB WDE6-PCB                             
054000                           XXDU-PCB  WDB6-PCB                             
                                 SYNQ-ATAB-PCB WDQ3-PCB.                        
054100     PERFORM IMS-GET-MSG                                                  
054200     IF SEGMENT-FINNS                                                     
054300        PERFORM A-INIT                                                    
054400        PERFORM B-GENERELL-KONTROLL                                       
054500        EVALUATE TRUE                                                     
054600        WHEN WS-INDATA-RATT                                               
054700           PERFORM C-RELATIONSKONTROLL                                    
054800           IF WS-INDATA-RATT                                              
054900              EVALUATE TRUE                                               
055000              WHEN WS-SAMMA-BILD                                          
055100                MOVE MID-IDTRANS-START TO MOD-IDTRANS-START               
055200                EVALUATE TRUE                                             
055300                WHEN MID-FLSVAR = JA OR YES                               
055400                  PERFORM D-KONTROLL-OK-JA                                
055500                WHEN MID-FLSVAR = NEJ                                     
055600                  PERFORM E-KONTROLL-OK-NEJ                               
055700                WHEN OTHER                                                
055800                  PERFORM F-KONTROLL-OK-BLANK                             
055900                END-EVALUATE                                              
056000              WHEN WS-GODKAND-BILD                                        
056100                PERFORM S02-LAGG-UT-RADER                                 
056200                MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAGGA-ATTR             
056300                MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLSVAR-ATTR             
056400              WHEN OTHER                                                  
056500                 PERFORM MFS-RENSA-FAELT-UT                               
056600              END-EVALUATE                                                
056700           END-IF                                                         
056800           MOVE MAX-MOD-LAENGD         TO MSG-KVLL                        
056900           MOVE '4318'                 TO MFS-IDTRANS                     
057000        WHEN WS-GODKAND-BILD                                              
057100          MOVE '4318'                 TO MFS-IDTRANS                      
057200          MOVE 'W4O31801'             TO MFS-IDMOD                        
057300          MOVE +130                   TO MSG-KVLL                         
057400          MOVE FEL                    TO WS-INDATA-TEST                   
057500        WHEN OTHER                                                        
057600          MOVE '4318'                 TO MFS-IDTRANS                      
057700          MOVE 'W4O31801'             TO MFS-IDMOD                        
057800          MOVE +130                   TO MSG-KVLL                         
057900          MOVE FEL                    TO WS-INDATA-TEST                   
058000          PERFORM MFS-RENSA-FAELT-UT                                      
058100          MOVE MFS-STAENG-FAELT      TO MOD-FLSVAR-ATTR                   
058200                                        MOD-FLAGGA-ATTR                   
058300        END-EVALUATE                                                      
058400        IF WS-INDATA-RATT                                                 
058500           PERFORM H-AVSLUT                                               
058600        END-IF                                                            
058700        IF MFS-IDTRANS = '4318'                                           
058800           PERFORM IMS-INSERT-MSG                                         
058900        END-IF                                                            
059000     END-IF                                                               
059100     MOVE ZERO TO RETURN-CODE                                             
059200     GOBACK                                                               
059300     .                                                                    
059400     EJECT                                                                
059500 A-INIT             SECTION.                                              
059600                                                                          
059700     IF MSG-DUBBLA-TRANSKODER                                             
059800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I31801               
059900       MOVE MSG-IDTRANS-2                 TO   MFS-IDTRANS                
060000                                               WS-IDTRANS                 
060100       MOVE MSG-KDMFSFOR-2                TO   MFS-KDMFSFOR               
060200                                               WS-KDMFSFOR                
060300       MOVE MSG-KDTRTYP                   TO   MFS-KDTRTYP                
060400       MOVE MSG-IDPFK                     TO   MFS-IDPFK                  
060500*      MOVE '+'                           TO   MID-FLAGGA                 
060600       MOVE '+'                           TO   MID-FLSVAR                 
060700     ELSE                                                                 
060800       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I31801               
060900       MOVE MSG-IDTRANS-1                 TO   MFS-IDTRANS                
061000                                               WS-IDTRANS                 
061100       MOVE MSG-KDMFSFOR-1                TO   MFS-KDMFSFOR               
061200                                               WS-KDMFSFOR                
061300       MOVE ' '                           TO   MFS-KDTRTYP                
061400     END-IF                                                               
061500                                                                          
061600     MOVE LOW-VALUE                       TO   MSG-AREA                   
061700     MOVE 'W4O31801'                      TO   MFS-IDMOD                  
061800     MOVE '4318'                          TO   MOD-IDTRANS                
061900                                                                          
062000     MOVE RAETT                           TO WS-INDATA-TEST               
062100                                             WS-BEHANDLING-TEST           
062200     MOVE NEJ                             TO WS-4397-STARTAD-SW           
062300                                                                          
062400     PERFORM AA-FLYTTA-NYCKLAR                                            
062500     PERFORM AB-SATT-FLAGGA-NYA-NYCKLAR                                   
062600                                                                          
062700     PERFORM MFS-RENSA-FAELT-IN                                           
062800     MOVE MFS-ROER-EJ-FAELT               TO   MOD-FLAGGA                 
062900                                               MOD-FLSVAR                 
063000     IF NOT WS-GODKAND-BILD                                               
063100       PERFORM MFS-RENSA-FAELT-UT                                         
063200     END-IF                                                               
063300     .                                                                    
063400     EJECT                                                                
063500 AA-FLYTTA-NYCKLAR  SECTION.                                              
063600                                                                          
063700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
063800     MOVE '001'             TO MSGI-KDCALL                                
063900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
064000     MOVE '4318'            TO MSGI-IDTRANS                               
064100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
064200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
064300                                                                          
064400     IF ENGLISH-TEXT                                                      
064500       MOVE +2 TO INDX                                                    
064600     ELSE                                                                 
064700       MOVE +1 TO INDX                                                    
064800     END-IF                                                               
064900                                                                          
065000     IF MID-IDANSTNR-IN = ALL '+'                                         
065100         MOVE MID-IDANSTNR-UT             TO   WS-IDANSTNR                
065200         INSPECT WS-IDANSTNR REPLACING LEADING SPACE BY ZERO              
065300     ELSE                                                                 
065400         MOVE MID-IDANSTNR-IN             TO   WS-IDANSTNR                
065500     END-IF                                                               
065600     SKIP2                                                                
065700     IF MID-IDPRODNR-IN = ALL '+'                                         
065800         MOVE MID-IDPRODNR-UT             TO   WS-IDPRODNR                
065900         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
066000     ELSE                                                                 
066100         MOVE MID-IDPRODNR-IN             TO   WS-IDPRODNR                
066200     END-IF                                                               
066300     SKIP2                                                                
066400     IF MID-IDDISTR-IN = ALL '+'                                          
066500         MOVE MID-IDDISTR-UT              TO   WS-IDDISTR                 
066600         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
066700     ELSE                                                                 
066800         MOVE MID-IDDISTR-IN              TO   WS-IDDISTR                 
066900     END-IF                                                               
067000     SKIP2                                                                
067100     IF MID-IDKUNDNR-IN = ALL '+'                                         
067200         MOVE MID-IDKUNDNR-UT             TO   WS-IDKUNDNR                
067300         INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
067400     ELSE                                                                 
067500         MOVE MID-IDKUNDNR-IN             TO   WS-IDKUNDNR                
067600     END-IF                                                               
067700     SKIP2                                                                
067800     IF MID-IDORDNR-IN = ALL '+'                                          
067900         MOVE MID-IDORDNR-UT              TO   WS-IDORDNR                 
068000         INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO               
068100     ELSE                                                                 
068200         MOVE MID-IDORDNR-IN              TO   WS-IDORDNR                 
068300     END-IF                                                               
068400     SKIP2                                                                
068500     IF MID-IDKOLLI-IN = ALL '+'                                          
068600         MOVE MID-IDKOLLI-UT              TO   WS-IDKOLLI                 
068700         INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO               
068800     ELSE                                                                 
068900         MOVE MID-IDKOLLI-IN              TO   WS-IDKOLLI                 
069000     END-IF                                                               
069100                                                                          
069200     MOVE MSGI-IDDC                       TO WS-IDDC                      
069300                                                                          
069400     MOVE MID-ADFLGEO                     TO   ARB-ADFLGEO                
069500     MOVE MID-ADFLOMR                     TO   ARB-ADFLOMR                
069600     MOVE MID-ADRUTNIV                    TO   ARB-ADRUTNIV               
069700                                                                          
069800     IF INDX = 1                                                          
069900       IF MID-ADFLGEO = SPACE       OR                                    
070000         (MID-ADFLOMR NOT NUMERIC)  OR                                    
070100         (MID-ADRUTNIV NOT NUMERIC)                                       
070200          MOVE MFS-RENSA-FAELT TO MOD-TEPLATS                             
070300       ELSE                                                               
070400          STRING 'KOLLIT UPPD, NOTERA PLATS: ' ARB-ADRESS                 
070500          DELIMITED BY SIZE INTO MOD-TEPLATS                              
070600       END-IF                                                             
070700     ELSE                                                                 
070800       MOVE MFS-RENSA-FAELT TO MOD-TEPLATS                                
070900     END-IF                                                               
071000                                                                          
071100     MOVE WS-IDANSTNR                     TO   MOD-IDANSTNR-UT            
071200     INSPECT MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE              
071300     MOVE WS-IDDISTR                      TO   MOD-IDDISTR-UT             
071400     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
071500     MOVE WS-IDKUNDNR                     TO   MOD-IDKUNDNR-UT            
071600     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
071700     MOVE WS-IDORDNR                      TO   MOD-IDORDNR-UT             
071800     INSPECT MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE              
071900     MOVE WS-IDKOLLI                      TO   MOD-IDKOLLI-UT             
072000     INSPECT MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE              
072100     MOVE WS-IDPRODNR                     TO   MOD-IDPRODNR-UT            
072200     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
072300     MOVE WS-IDDC                         TO   MOD-IDDC-UT                
072400                                                                          
072500     IF WS-IDPLKLST-SPAR = ZERO                                           
072600        MOVE 001                          TO   MOD-IDPLKLST-SPAR          
072700     ELSE                                                                 
072800        MOVE WS-IDPLKLST-SPAR             TO   MOD-IDPLKLST-SPAR          
072900     END-IF                                                               
073000     MOVE WS-IDRADNR-SPAR                 TO   MOD-IDRADNR-SPAR           
073100     .                                                                    
073200     EJECT                                                                
073300 AB-SATT-FLAGGA-NYA-NYCKLAR              SECTION.                         
073400                                                                          
073500     IF MID-IDANSTNR-IN NOT = ALL '+' OR                                  
073600        MID-IDPRODNR-IN NOT = ALL '+' OR                                  
073700        MID-IDDISTR-IN NOT = ALL '+'  OR                                  
073800        MID-IDKUNDNR-IN NOT = ALL '+' OR                                  
073900        MID-IDORDNR-IN NOT = ALL '+'  OR                                  
074000        NOT WS-SAMMA-BILD                                                 
074100        MOVE SPACE              TO  MFS-IDPFK                             
074200        MOVE JA                 TO  FL-NYCKLAR                            
074300        MOVE ZERO               TO  MID-IDPLKLST-SPAR                     
074400                                    MID-IDRADNR-SPAR                      
074500     ELSE                                                                 
074600        MOVE NEJ                TO  FL-NYCKLAR                            
074700     END-IF                                                               
074800     .                                                                    
074900     EJECT                                                                
075000 B-GENERELL-KONTROLL  SECTION.                                            
075100     SKIP3                                                                
075200     IF WS-IDANSTNR NOT NUMERIC                                           
075300     OR WS-IDANSTNR =   ZERO                                              
075400         MOVE FEL                       TO   WS-INDATA-TEST               
075500         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
075600     END-IF                                                               
075700     SKIP2                                                                
075800     IF WS-IDDISTR  NOT NUMERIC                                           
075900         MOVE FEL                       TO   WS-INDATA-TEST               
076000         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
076100     ELSE                                                                 
076200         MOVE WS-IDDISTR                TO   W-401-IDDISTR                
076300     END-IF                                                               
076400     SKIP2                                                                
076500     IF WS-IDKUNDNR NOT NUMERIC                                           
076600         MOVE FEL                       TO   WS-INDATA-TEST               
076700         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
076800     ELSE                                                                 
076900         MOVE WS-IDKUNDNR               TO   W-401-IDKUNDNR               
077000     END-IF                                                               
077100     SKIP2                                                                
077200     IF WS-IDORDNR  NOT NUMERIC                                           
077300         MOVE FEL                       TO   WS-INDATA-TEST               
077400         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
077500     ELSE                                                                 
077600         MOVE WS-IDORDNR                TO   W-401-IDORDNR                
077700     END-IF                                                               
077800     SKIP2                                                                
077900     IF WS-IDPRODNR NOT NUMERIC                                           
078000         MOVE FEL                       TO   WS-INDATA-TEST               
078100         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
078200     END-IF                                                               
078300                                                                          
078400                                                                          
078500     IF WS-IDDC IS > SPACE                                                
078600*      CONTINUE                                                           
078700        MOVE WS-IDDC TO W-IDDC-B6                                         
078800        PERFORM IMS-GU-WDB601                                             
078900     ELSE                                                                 
079000       MOVE FEL                           TO WS-INDATA-TEST               
079100       MOVE FEL-749 (INDX)                TO MOD-TEMFSFEL                 
079200     END-IF                                                               
079300                                                                          
079400     IF WS-SAMMA-BILD                                                     
079500       PERFORM BA-KOLL-MID-FLAGGA                                         
079600       PERFORM BB-KOLL-MID-FLSVAR                                         
079700     ELSE                                                                 
079800       MOVE '+'                   TO MID-FLAGGA                           
079900                                     MID-FLSVAR                           
080000       MOVE MFS-RENSA-FAELT       TO MOD-FLAGGA                           
080100                                     MOD-FLSVAR                           
080200       MOVE MFS-STAENG-FAELT      TO MOD-FLAGGA-ATTR                      
080300                                     MOD-FLSVAR-ATTR                      
080400     END-IF                                                               
080500     IF  MID-IDRADNR-25 NOT = ALL '+'                                     
080600         MOVE MID-IDRADNR-25            TO WS-IDRADNR-25                  
080700         INSPECT WS-IDRADNR-25 REPLACING LEADING SPACE BY ZERO            
080800     ELSE                                                                 
080900         MOVE ZERO                      TO WS-IDRADNR-25                  
081000     END-IF                                                               
081100     .                                                                    
081200     EJECT                                                                
081300 BA-KOLL-MID-FLAGGA SECTION.                                              
081400*MID-FLAGGA = JA BETYDER ATT MAN VILL SE NÄSTA PLOCKLISTA OCH EV.         
081500*OPACKADE RADER.                                                          
081600                                                                          
081700     EVALUATE TRUE                                                        
081800     WHEN MID-FLAGGA = ALL '+'                                            
081900         MOVE MFS-RENSA-FAELT            TO MOD-FLAGGA                    
082000         MOVE MFS-ALFA-FAELT-RAETT       TO MOD-FLAGGA-ATTR               
082100     WHEN MID-FLAGGA = ALL ' '                                            
082200         MOVE MFS-RENSA-FAELT            TO MOD-FLAGGA                    
082300         MOVE MFS-ALFA-FAELT-RAETT       TO MOD-FLAGGA-ATTR               
082400     WHEN MID-FLAGGA = 'X' OR JA                                          
082500         IF FL-NYCKLAR = JA                                               
082600            CONTINUE                                                      
082700         ELSE                                                             
082800            IF MID-FLSVAR = 'J' OR 'Y'                                    
082900               MOVE MFS-ROER-EJ-FAELT    TO MOD-FLAGGA                    
083000               MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAGGA-ATTR               
083100               MOVE FEL                  TO WS-INDATA-TEST                
083200               MOVE FEL-748 (INDX)       TO MOD-TEMFSFEL                  
083300            ELSE                                                          
083400               MOVE '+'                        TO MID-FLSVAR              
083500               MOVE MFS-ROER-EJ-FAELT    TO MOD-FLAGGA                    
083600               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-ATTR               
083700            END-IF                                                        
083800         END-IF                                                           
083900     WHEN OTHER                                                           
084000         MOVE MFS-ROER-EJ-FAELT          TO MOD-FLAGGA                    
084100         MOVE MFS-ALFA-FAELT-FEL         TO MOD-FLAGGA-ATTR               
084200         MOVE FEL                        TO WS-INDATA-TEST                
084300         MOVE FEL-748 (INDX)             TO MOD-TEMFSFEL                  
084400     END-EVALUATE                                                         
084500     .                                                                    
084600     EJECT                                                                
084700 BB-KOLL-MID-FLSVAR SECTION.                                              
084800* GODKÄNN EV. AVSLUT AV EN ORDERDEL.                                      
084900                                                                          
085000     EVALUATE TRUE                                                        
085100     WHEN MID-FLSVAR = ALL '+'                                            
085200         MOVE MFS-RENSA-FAELT         TO MOD-FLSVAR                       
085300         MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLSVAR-ATTR                  
085400     WHEN MID-FLSVAR = ALL ' '                                            
085500         MOVE MFS-RENSA-FAELT         TO MOD-FLSVAR                       
085600         MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLSVAR-ATTR                  
085700     WHEN MID-FLSVAR = 'J' OR 'Y' OR 'N'                                  
085800         IF FL-NYCKLAR = JA                                               
085900            CONTINUE                                                      
086000         ELSE                                                             
086100            MOVE MFS-ROER-EJ-FAELT    TO MOD-FLSVAR                       
086200            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSVAR-ATTR                  
086300         END-IF                                                           
086400     WHEN OTHER                                                           
086500         MOVE MFS-ROER-EJ-FAELT       TO MOD-FLSVAR                       
086600         MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLSVAR-ATTR                  
086700         MOVE FEL                     TO WS-INDATA-TEST                   
086800         MOVE FEL-748 (INDX)          TO MOD-TEMFSFEL                     
086900     END-EVALUATE                                                         
087000     .                                                                    
087100     EJECT                                                                
087200 C-RELATIONSKONTROLL  SECTION.                                            
087300     SKIP3                                                                
087400     MOVE WS-IDDISTR                    TO WS-IDDISTR-NUM                 
087500     MOVE WS-IDKUNDNR                   TO WS-IDKUNDNR-NUM                
087600                                                                          
087700     PERFORM CA-KONTROLLERA-KUNDORDNR                                     
087800     SKIP2                                                                
087900     IF WS-INDATA-RATT                                                    
088000         PERFORM CB-KONTROLLERA-PACKARE                                   
088100     END-IF                                                               
088200     IF WS-IDPLKLST-SPAR = ZERO                                           
088300        MOVE 001              TO MOD-IDPLKLST-SPAR                        
088400                                 WS-IDPLKLST-SPAR                         
088500     ELSE                                                                 
088600        MOVE WS-IDPLKLST-SPAR TO MOD-IDPLKLST-SPAR                        
088700     END-IF                                                               
088800     MOVE WS-IDRADNR-SPAR  TO MOD-IDRADNR-SPAR                            
088900     IF (MID-FLSVAR = 'J' OR 'Y' OR 'N') AND                              
089000         FL-NYCKLAR = JA                                                  
089100            MOVE MFS-RENSA-FAELT         TO MOD-FLSVAR                    
089200                                            MOD-FLAGGA                    
089300            MOVE MFS-STAENG-FAELT        TO MOD-FLSVAR-ATTR               
089400                                            MOD-FLAGGA-ATTR               
089500            MOVE FEL                     TO WS-INDATA-TEST                
089600            MOVE FEL-787 (INDX)          TO MOD-TEMFSFEL                  
089700     END-IF                                                               
089800     IF ((MID-FLAGGA = 'X' OR JA)  AND                                    
089900          FL-NYCKLAR = JA)                                                
090000            MOVE MFS-RENSA-FAELT         TO MOD-FLSVAR                    
090100                                            MOD-FLAGGA                    
090200            MOVE MFS-STAENG-FAELT        TO MOD-FLSVAR-ATTR               
090300                                            MOD-FLAGGA-ATTR               
090400            MOVE FEL                     TO WS-INDATA-TEST                
090500            MOVE FEL-787 (INDX)          TO MOD-TEMFSFEL                  
090600     END-IF                                                               
090700     .                                                                    
090800     EJECT                                                                
090900 CA-KONTROLLERA-KUNDORDNR SECTION.                                        
091000     SKIP3                                                                
091100     IF MID-IDPRODNR-IN = ALL '+'                                         
091200         IF    MID-IDDISTR-IN  = ALL '+'                                  
091300           AND MID-IDKUNDNR-IN = ALL '+'                                  
091400           AND MID-IDORDNR-IN  = ALL '+'                                  
091500             IF WS-IDPRODNR > ZERO                                        
091600*--------------------------ANVÄNDS GAMLA PRODNR: MID-IDPRODNR-UT          
091700                 MOVE JA              TO  SOEK-VIA-PRODNR                 
091800                 MOVE MFS-RENSA-FAELT TO  MOD-IDDISTR-UT                  
091900                                          MOD-IDKUNDNR-UT                 
092000                                          MOD-IDORDNR-UT                  
092100             ELSE                                                         
092200                 PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                        
092300                 MOVE MFS-RENSA-FAELT TO  MOD-IDPRODNR-UT                 
092400             END-IF                                                       
092500         ELSE                                                             
092600             MOVE JA                  TO FL-NYCKLAR                       
092700             PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                            
092800             MOVE MFS-RENSA-FAELT     TO  MOD-IDPRODNR-UT                 
092900         END-IF                                                           
093000     ELSE                                                                 
093100         MOVE JA                       TO SOEK-VIA-PRODNR                 
093200         MOVE JA                       TO FL-NYCKLAR                      
093300         MOVE MFS-RENSA-FAELT          TO MOD-IDDISTR-UT                  
093400                                          MOD-IDKUNDNR-UT                 
093500                                          MOD-IDORDNR-UT                  
093600     END-IF                                                               
093700     SKIP2                                                                
093800     IF WS-INDATA-RATT                                                    
093900         PERFORM CAB-KOLLA-MOT-WDE411                                     
094000     END-IF                                                               
094100     .                                                                    
094200     EJECT                                                                
094300 CAA-HAMTA-PRODNR-I-WDE4-6  SECTION.                                      
094400     SKIP3                                                                
094500     MOVE 'N'                             TO WS-SLINGA-KLAR               
094600                                                                          
094700     MOVE WS-IDDISTR-NUM                  TO W-4A1-IDDISTR                
094800     MOVE WS-IDKUNDNR-NUM                 TO W-4A1-IDKUNDNR               
094900     MOVE WS-IDORDNR                      TO W-4A1-IDORDNR                
095000     PERFORM IMS-GU-KUNDORDER-SEK                                         
095100                                                                          
095200     IF KUNDORDER-SEK-FINNS                                               
095300        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
095400                      SLINGA-KLAR                                         
095500        MOVE KORD-IDDISTR                 TO W-401-IDDISTR                
095600        MOVE KORD-IDKUNDNR                TO W-401-IDKUNDNR               
095700        MOVE KORD-IDORDNR5                TO W-401-IDORDNR                
095800        MOVE KORD-IDPRODNR                TO W-401-IDPRODNR               
095900        MOVE KORD-IDPLKLST                TO W-401-IDPLKLST               
096000        PERFORM IMS-GU-KUNDORDER                                          
096100        MOVE KORD-KVORDRAD-LEVPL   TO WS-KVORDRAD-LEVPL-SPAR              
096200                                                                          
096300        MOVE KORD-IDPRODNR         TO W-601-IDPRODNR                      
096400        PERFORM IMS-GU-IDPRODNR                                           
096500        IF SEGMENT-FINNS                                                  
096600           IF SOEK-VIA-PRODNR = JA                                        
096700              MOVE VORD-IDPRODNR TO WS-JFR-IDPRODNR                       
096800              IF WS-JFR-IDPRODNR = WS-IDPRODNR                            
096900                 CONTINUE                                                 
097000              ELSE                                                        
097100                 SET SEGMENT-SAKNAS TO TRUE                               
097200              END-IF                                                      
097300           ELSE                                                           
097400              IF VORD-IDDC = WS-IDDC AND                                  
097500                 WS-KVORDRAD-LEVPL-SPAR = ZERO                            
097600                 CONTINUE                                                 
097700              ELSE                                                        
097800                 SET SEGMENT-SAKNAS TO TRUE                               
097900              END-IF                                                      
098000           END-IF                                                         
098100        END-IF                                                            
098200        IF SEGMENT-FINNS                                                  
098300           IF VORD-IDDC = WS-IDDC                                         
098400              MOVE 'J'                 TO   WS-SLINGA-KLAR                
098500              MOVE VORD-IDPRODNR       TO   WS-IDPRODNR                   
098600           END-IF                                                         
098700        END-IF                                                            
098800                                                                          
098900        PERFORM IMS-GN-KUNDORDER-SEK                                      
099000        END-PERFORM                                                       
099100     ELSE                                                                 
099200        MOVE FEL                     TO   WS-INDATA-TEST                  
099300        MOVE FEL-701 (INDX)          TO   MOD-TEMFSFEL                    
099400        MOVE MFS-RENSA-FAELT         TO   MOD-FLSVAR                      
099500                                          MOD-FLAGGA                      
099600        MOVE MFS-STAENG-FAELT        TO   MOD-FLSVAR-ATTR                 
099700                                          MOD-FLAGGA-ATTR                 
099800     END-IF                                                               
099900                                                                          
100000     IF NOT SLINGA-KLAR                                                   
100100        MOVE FEL                         TO   WS-INDATA-TEST              
100200        MOVE FEL-701 (INDX)              TO   MOD-TEMFSFEL                
100300        MOVE MFS-RENSA-FAELT             TO   MOD-FLSVAR                  
100400                                              MOD-FLAGGA                  
100500        MOVE MFS-STAENG-FAELT            TO   MOD-FLSVAR-ATTR             
100600                                              MOD-FLAGGA-ATTR             
100700     END-IF                                                               
100800     .                                                                    
100900     EJECT                                                                
101000 CAB-KOLLA-MOT-WDE411       SECTION.                                      
101100     SKIP3                                                                
101200     MOVE WS-IDPRODNR                   TO W-411-IDPRODNR-MIN             
101300                                           W-411-IDPRODNR-MAX             
101400     MOVE 1                             TO W-411-IDPURAD-MIN              
101500     MOVE 99999                         TO W-411-IDPURAD-MAX              
101600     PERFORM IMS-GU-KUNDORDER-SEK-INV                                     
101700                                                                          
101800     IF SEGMENT-FINNS                                                     
101900        IF KORD-IDDC = WS-IDDC                                            
102000          MOVE KORD-IDDISTR             TO   WS-IDDISTR-NUM               
102100          MOVE KORD-KDORDKL             TO   WS-KDORDKL                   
102200          MOVE WS-IDDISTR-NUM           TO   WS-IDDISTR                   
102300                                               MOD-IDDISTR-UT             
102400          INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE          
102500          MOVE KORD-IDKUNDNR            TO   WS-IDKUNDNR-NUM              
102600          MOVE WS-IDKUNDNR-NUM          TO   WS-IDKUNDNR                  
102700                                               MOD-IDKUNDNR-UT            
102800          INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
102900          MOVE KORD-IDKUNDRF            TO   WS-IDKUNDRF                  
103000          MOVE WS-IDORDNR               TO   MOD-IDORDNR-UT               
103100          INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE          
103200        ELSE                                                              
103300           MOVE FEL                        TO   WS-INDATA-TEST            
103400           MOVE FEL-701 (INDX)             TO   MOD-TEMFSFEL              
103500           MOVE MFS-RENSA-FAELT            TO   MOD-FLSVAR                
103600                                                MOD-FLAGGA                
103700           MOVE MFS-STAENG-FAELT           TO   MOD-FLSVAR-ATTR           
103800                                                MOD-FLAGGA-ATTR           
103900        END-IF                                                            
104000     ELSE                                                                 
104100        MOVE FEL                        TO   WS-INDATA-TEST               
104200        MOVE FEL-701 (INDX)             TO   MOD-TEMFSFEL                 
104300        MOVE MFS-RENSA-FAELT            TO   MOD-FLSVAR                   
104400                                             MOD-FLAGGA                   
104500        MOVE MFS-STAENG-FAELT           TO   MOD-FLSVAR-ATTR              
104600                                             MOD-FLAGGA-ATTR              
104700     END-IF                                                               
104800     .                                                                    
104900     EJECT                                                                
105000 CB-KONTROLLERA-PACKARE SECTION.                                          
105100     SKIP3                                                                
105200     MOVE 'N'             TO WS-TRAEFF-PACKARE                            
105300     MOVE ZERO            TO WS-ANT-ODEL-KVAR-ATT-BEHANDLA                
105400     IF FL-NYA-NYCKLAR                                                    
105500       MOVE ZERO          TO WS-IDPLKLST-FOM                              
105600                             WS-IDRADNR-SPAR                              
105700     ELSE                                                                 
105800       EVALUATE TRUE                                                      
105900       WHEN MFS-IDPFK = '7'                                               
106000         IF MID-FLAGGA = 'X' OR JA                                        
106100           MOVE ZERO  TO WS-IDPLKLST-FOM                                  
106200         ELSE                                                             
106300           COMPUTE WS-IDPLKLST-FOM = MID-IDPLKLST-SPAR - 1                
106400           END-COMPUTE                                                    
106500         END-IF                                                           
106600         MOVE ZERO    TO WS-IDRADNR-SPAR                                  
106700       WHEN MFS-IDPFK = ' ' OR '8'                                        
106800         IF MID-FLAGGA = 'X' OR JA                                        
106900           MOVE MID-IDPLKLST-SPAR TO WS-IDPLKLST-FOM                      
107000           MOVE ZERO              TO WS-IDRADNR-SPAR                      
107100         ELSE                                                             
107200           COMPUTE WS-IDPLKLST-FOM = MID-IDPLKLST-SPAR - 1                
107300           END-COMPUTE                                                    
107400           IF MID-IDRADNR-SPAR NUMERIC                                    
107500             MOVE MID-IDRADNR-SPAR TO WS-IDRADNR-SPAR                     
107600           ELSE                                                           
107700             MOVE ZERO             TO MID-IDRADNR-SPAR                    
107800           END-IF                                                         
107900         END-IF                                                           
108000       END-EVALUATE                                                       
108100     END-IF                                                               
108200     PERFORM S01-HAEMTA-PLKLST-EJ-KLAR                                    
108300     .                                                                    
108400     EJECT                                                                
108500 D-KONTROLL-OK-JA SECTION.                                                
108600     SKIP3                                                                
108700     EVALUATE TRUE                                                        
108800     WHEN FL-NYA-NYCKLAR                                                  
108900        MOVE FEL-787 (INDX) TO MOD-TEMFSFEL                               
109000        MOVE FEL            TO WS-BEHANDLING-TEST                         
109100        MOVE MFS-RENSA-FAELT    TO MOD-FLSVAR                             
109200        MOVE MFS-STAENG-FAELT   TO MOD-FLSVAR-ATTR                        
109300     WHEN WS-IDRADNR-25 NOT = ZERO                                        
109400*                            EV = SPAR?                                   
109500        MOVE FEL-819(INDX)      TO MOD-TEMFSFEL                           
109600        MOVE FEL            TO WS-BEHANDLING-TEST                         
109700        MOVE MFS-ROER-EJ-FAELT  TO MOD-FLSVAR                             
109800        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSVAR-ATTR                        
109900     WHEN OTHER                                                           
110000        PERFORM DA-EV-STARTA-4397                                         
110100     END-EVALUATE                                                         
110200     .                                                                    
110300     EJECT                                                                
110400 DA-EV-STARTA-4397                       SECTION.                         
110500                                                                          
110600     MOVE WS-IDDISTR       TO W-401-IDDISTR                               
110700     MOVE WS-IDKUNDNR      TO W-401-IDKUNDNR                              
110800     MOVE WS-IDORDNR       TO W-401-IDORDNR                               
110900     MOVE WS-IDPRODNR      TO W-401-IDPRODNR                              
111000     MOVE WS-IDPLKLST-SPAR TO W-401-IDPLKLST                              
111100     PERFORM IMS-GHU-KUNDORDER                                            
111200                                                                          
111300     IF WS-SAMMA-BILD     AND                                             
111400        FL-GAMLA-NYCKLAR  AND                                             
111500        KORD-KDPAKOLL = 0                                                 
111600       MOVE FEL                TO WS-INDATA-TEST                          
111700       MOVE FEL-8171(INDX)     TO MOD-TEMFSFEL                            
111800       MOVE MFS-ROER-EJ-FAELT  TO MOD-FLSVAR                              
111900       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSVAR-ATTR                         
112000     ELSE                                                                 
112100       IF KORD-FLPAFEL = NEJ                                              
112200          PERFORM DAA-STARTA-4397                                         
112300       ELSE                                                               
112400          PERFORM DAB-RADER-KVAR-ATT-RAPPORTERA                           
112500          MOVE +0    TO KORD-KDPAKOLL                                     
112600          MOVE NEJ   TO KORD-FLPAFEL                                      
112700          PERFORM IMS-REPL-KUNDORDER                                      
112800       END-IF                                                             
112900     END-IF                                                               
113000     .                                                                    
113100     EJECT                                                                
113200 DAA-STARTA-4397                         SECTION.                         
113300                                                                          
113400     MOVE +2                TO KORD-KDPAKOLL                              
113500     PERFORM IMS-REPL-KUNDORDER                                           
113600     PERFORM DAAA-SKAPA-BAKGRUNDTRANS                                     
113700     MOVE MFS-RENSA-FAELT      TO MOD-FLSVAR                              
113800                                                                          
113900     IF WS-IDPLKLST-NAESTA > 0                                            
114000       MOVE WS-IDPLKLST-NAESTA   TO WS-IDPLKLST-SPAR                      
114100                                    MOD-IDPLKLST-SPAR                     
114200       MOVE ZERO                 TO WS-IDRADNR-SPAR                       
114300       MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSVAR-ATTR                       
114400       PERFORM S02-LAGG-UT-RADER                                          
114500     ELSE                                                                 
114600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSVAR-ATTR                       
114700     END-IF                                                               
114800     .                                                                    
114900     EJECT                                                                
115000 DAAA-SKAPA-BAKGRUNDTRANS  SECTION.                                       
115100     SKIP3                                                                
           MOVE ZERO TO W-411-IDPURAD                                           
           PERFORM IMS-GNP-RAD-KVAL                                             
           PERFORM UNTIL SEGMENT-SAKNAS                                         
            IF WS-IDDC = 11 AND                                                 
               (ORAD-ADLAGOMR = 90 OR 98) AND                                   
               ORAD-KDRADSTA < 4                                                
             MOVE 'PCK' TO SYNQC-ORDERTYPE                                      
             MOVE WS-IDPLKLST-SPAR TO SYNQC-IDPLKLST                            
             MOVE WS-IDPRODNR TO SYNQC-IDPRODNR                                 
             MOVE ORAD-IDPURAD TO SYNQC-IDRADNR                                 
             CALL W488ORCN USING  SYNQC-W488ORCN SYNQ-PCB                       
                                SYNQ-ATAB-PCB WDQ3-PCB                          
            END-IF                                                              
            PERFORM IMS-GNP-RAD-OKVAL                                           
           END-PERFORM                                                          
115200     MOVE 'W4T397X '              TO  ALT-TRANSKOD                        
115300                                      ALT-LTERM-NAME                      
115400     MOVE WS-KDMFSFOR             TO  ALT-KDMFSFOR                        
115500     MOVE '4318'                  TO  ALT-IDTRANS                         
115600     MOVE +117                    TO  ALT-LL                              
115700     MOVE WS-IDPRODNR             TO  4397-IDPRODNR                       
115800     MOVE WS-IDANSTNR             TO  4397-IDANSTNR                       
115900     MOVE WS-IDPLKLST-SPAR        TO  4397-IDPLKLST                       
116000     MOVE ZERO                    TO  4397-IDPURAD                        
116100     MOVE 4397-TRANSAREA          TO  ALT-AREA                            
116200     PERFORM IMS-INSERT-ALTMSG                                            
116300     MOVE JA                      TO WS-4397-STARTAD-SW                   
116400     .                                                                    
116500     EJECT                                                                
116600 DAB-RADER-KVAR-ATT-RAPPORTERA           SECTION.                         
116700                                                                          
116800     MOVE MFS-ROER-EJ-FAELT    TO MOD-FLSVAR                              
116900     MOVE MFS-ALFA-FAELT-FEL   TO                                         
117000                            MOD-FLSVAR-ATTR                               
117100     MOVE FEL-816 (INDX) TO MOD-TEMFSFEL                                  
117200     MOVE FEL           TO WS-BEHANDLING-TEST                             
117300     MOVE UPPL-3 (INDX) TO MOD-TEMFSINF                                   
117400     .                                                                    
117500     EJECT                                                                
117600 E-KONTROLL-OK-NEJ SECTION.                                               
117700                                                                          
117800     MOVE WS-IDDISTR        TO W-401-IDDISTR                              
117900     MOVE WS-IDKUNDNR       TO W-401-IDKUNDNR                             
118000     MOVE WS-IDORDNR        TO W-401-IDORDNR                              
118100     MOVE WS-IDPRODNR       TO W-401-IDPRODNR                             
118200     MOVE MID-IDPLKLST-SPAR TO W-401-IDPLKLST                             
118300     PERFORM IMS-GHU-KUNDORDER-GODK-GE                                    
118400                                                                          
118500     IF SEGMENT-FINNS                                                     
118600       MOVE +0         TO KORD-KDPAKOLL                                   
118700       MOVE NEJ        TO KORD-FLPAFEL                                    
118800       PERFORM IMS-REPL-KUNDORDER                                         
118900     END-IF                                                               
119000                                                                          
119100     MOVE MFS-RENSA-FAELT TO MOD-FLSVAR                                   
119200     .                                                                    
119300     EJECT                                                                
119400 F-KONTROLL-OK-BLANK SECTION.                                             
119500     SKIP3                                                                
119600     MOVE MFS-RENSA-FAELT TO MOD-FLAGGA                                   
119700     PERFORM S02-LAGG-UT-RADER                                            
119800     MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAGGA-ATTR                         
119900     MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSVAR-ATTR                         
120000* -----------  FÖR ATT GE UPPLYSTA FÄLT   ----------------------          
120100     .                                                                    
120200     EJECT                                                                
120300 H-AVSLUT             SECTION.                                            
120400                                                                          
120500     IF ((MID-FLSVAR = JA OR YES OR NEJ)  OR                              
120600         (MID-FLAGGA = JA OR 'X'))        AND                             
120700          WS-SAMMA-BILD                                                   
120800         IF WS-BEHANDLING-RATT                                            
120900           IF ((MID-FLSVAR = JA OR YES)             AND                   
121000                WS-ANT-ODEL-KVAR-ATT-BEHANDLA < 2)  OR                    
121100                MID-FLSVAR = NEJ                                          
121200             MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLSVAR-ATTR              
121300             MOVE MFS-RENSA-FAELT         TO MOD-FLSVAR                   
121400             IF  MID-FLSVAR = JA OR YES                                   
121500                 MOVE RAETT-1 (INDX)      TO  MOD-TEMFSINF                
121600             ELSE                                                         
121700                 MOVE RAETT-2 (INDX)      TO  MOD-TEMFSINF                
121800                 IF MID-IDTRANS-START NOT = '4314' AND '4315'             
121900                    MOVE MFS-RENSA-FAELT  TO  MOD-IDANSTNR-UT             
122000                                              MOD-IDDISTR-UT              
122100                                              MOD-IDKUNDNR-UT             
122200                                              MOD-IDORDNR-UT              
122300                                              MOD-IDKOLLI-UT              
122400                                              MOD-IDPRODNR-UT             
122500                                              MOD-FLSVAR                  
122600                                              MOD-FLAGGA                  
122700                    MOVE MFS-STAENG-FAELT  TO MOD-FLSVAR-ATTR             
122800                                              MOD-FLAGGA-ATTR             
122900                 END-IF                                                   
123000             END-IF                                                       
123100             IF MID-IDRADNR-25 = ALL '+'                                  
123200                 IF MID-IDTRANS-START =  '4315'                           
123300                     MOVE 'W4O31501'      TO  MFS-IDMOD                   
123400                     MOVE '4318'          TO  MFS-IDTRANS                 
123500                     MOVE 4315-MOD-LAENGD TO  MSG-KVLL                    
123600                     PERFORM HB-LADDA-4315                                
123700                     MOVE 4315-MOD        TO  MSG-AREA                    
123800                 END-IF                                                   
123900                 IF MID-IDTRANS-START =  '4314'                           
124000                     MOVE 'W4O31401'      TO  MFS-IDMOD                   
124100                     MOVE '4318'          TO  MFS-IDTRANS                 
124200                     MOVE 4314-MOD-LAENGD TO  MSG-KVLL                    
124300                     PERFORM HA-LADDA-4314                                
124400                     MOVE 4314-MOD        TO  MSG-AREA                    
124500                 END-IF                                                   
124600             END-IF                                                       
124700           END-IF                                                         
124800         ELSE                                                             
124900             PERFORM HC-SAMMA-BILD                                        
125000         END-IF                                                           
125100     END-IF                                                               
125200                                                                          
125300     IF MID-FLSVAR = '+' OR SPACE                                         
125400       MOVE 'N'                    TO MOD-FLSVAR                          
125500     END-IF                                                               
125600     .                                                                    
125700     EJECT                                                                
125800 HA-LADDA-4314        SECTION.                                            
125900     SKIP3                                                                
126000     MOVE LOW-VALUE                TO 4314-MOD                            
126100     MOVE '4314'                   TO 4314-MOD-IDTRANS                    
126200                                      4314-MOD-IDTRANS-START              
126300     MOVE SPACE                    TO 4314-MOD-TEMFSFEL                   
126400     MOVE MFS-RENSA-FAELT          TO 4314-MOD-IDANSTNR-IN                
126500                                      4314-MOD-IDDISTR-IN                 
126600                                      4314-MOD-IDKUNDNR-IN                
126700                                      4314-MOD-IDORDNR-IN                 
126800                                      4314-MOD-IDKOLLI-IN                 
126900                                      4314-MOD-IDPRODNR-IN                
127000                                      4314-MOD-FLFORTSK                   
127100                                      4314-MOD-FLSISTAK                   
127200                                      4314-MOD-IDRADNR-FOM-S              
127300                                      4314-MOD-IDRADNR-TOM-S              
127400                                      4314-MOD-KVLEVART-S                 
127500     MOVE WS-IDANSTNR              TO 4314-MOD-IDANSTNR-UT                
127600     INSPECT 4314-MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE         
127700     MOVE WS-IDDISTR               TO 4314-MOD-IDDISTR-UT                 
127800     INSPECT 4314-MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE         
127900     MOVE WS-IDKUNDNR              TO 4314-MOD-IDKUNDNR-UT                
128000     INSPECT 4314-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
128100     MOVE WS-IDORDNR               TO 4314-MOD-IDORDNR-UT                 
128200     INSPECT 4314-MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE         
128300     MOVE WS-IDKOLLI               TO 4314-MOD-IDKOLLI-UT                 
128400     INSPECT 4314-MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE         
128500     MOVE WS-IDPRODNR              TO 4314-MOD-IDPRODNR-UT                
128600     INSPECT 4314-MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE         
128700     MOVE MOD-TEMFSINF             TO 4314-MOD-TEMFSINF                   
128800     MOVE WS-IDDC                  TO 4314-MOD-IDDC-UT                    
128900     SKIP2                                                                
129000     MOVE +1                       TO INX                                 
129100     PERFORM UNTIL INX NOT < 13                                           
129200         MOVE MFS-RENSA-FAELT      TO 4314-MOD-IDRADNR-FOM (INX)          
129300                                      4314-MOD-IDRADNR-TOM (INX)          
129400                                      4314-MOD-KVLEVART (INX)             
129500         ADD +1 TO INX                                                    
129600     END-PERFORM                                                          
129700     .                                                                    
129800     EJECT                                                                
129900 HB-LADDA-4315        SECTION.                                            
130000     SKIP3                                                                
130100     MOVE LOW-VALUE                TO 4315-MOD                            
130200     MOVE '4315'                   TO 4315-MOD-IDTRANS                    
130300                                      4315-MOD-IDTRANS-START              
130400     MOVE SPACE                    TO 4315-MOD-TEMFSFEL                   
130500     MOVE MFS-RENSA-FAELT          TO 4315-MOD-IDANSTNR-IN                
130600                                      4315-MOD-IDDISTR-IN                 
130700                                      4315-MOD-IDKUNDNR-IN                
130800                                      4315-MOD-IDORDNR-IN                 
130900                                      4315-MOD-IDKOLLI-IN                 
131000                                      4315-MOD-IDPRODNR-IN                
131100                                      4315-MOD-FLSISTAK                   
131200                                      4315-MOD-KDKOLLI                    
131300                                      4315-MOD-VKORDBTO-KOLLI             
131400                                      4315-MOD-KDEMBTYP                   
131500                                      4315-MOD-DIKOLLIL                   
131600                                      4315-MOD-DIKOLLIB                   
131700                                      4315-MOD-DIKOLLIH                   
131800                                      4315-MOD-ADFLGEO                    
131900                                      4315-MOD-ADFLOMR                    
132000                                      4315-MOD-ADRUTNIV                   
132100                                      4315-MOD-IDKOLLI-FOM                
132200                                      4315-MOD-IDKOLLI-TOM                
132300     MOVE MSGI-KDPRTVAL-ADR        TO 4315-MOD-PRTVAL-ADRESSFL            
132400     MOVE MSGI-KDPRTVAL-FS         TO 4315-MOD-PRTVAL-FOLJEFL             
132500     IF SDC OR NDC-NA                                                     
132600        MOVE MFS-CLOSE-FIELD-NOMOD TO 4315-MOD-PRTVAL-FOLJEFL-ATTR        
132700     END-IF                                                               
132800     MOVE WS-IDANSTNR              TO 4315-MOD-IDANSTNR-UT                
132900     INSPECT 4315-MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE         
133000     MOVE WS-IDDISTR               TO 4315-MOD-IDDISTR-UT                 
133100     INSPECT 4315-MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE         
133200     MOVE WS-IDKUNDNR              TO 4315-MOD-IDKUNDNR-UT                
133300     INSPECT 4315-MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
133400     MOVE WS-IDORDNR               TO 4315-MOD-IDORDNR-UT                 
133500     INSPECT 4315-MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE         
133600     MOVE WS-IDKOLLI               TO 4315-MOD-IDKOLLI-UT                 
133700     INSPECT 4315-MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE         
133800     MOVE WS-IDPRODNR              TO 4315-MOD-IDPRODNR-UT                
133900     INSPECT 4315-MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE         
134000     MOVE MOD-TEMFSINF             TO 4315-MOD-TEMFSINF                   
134100     MOVE WS-IDDC                  TO 4315-MOD-IDDC-UT                    
134200     MOVE +1                       TO INX                                 
134300     PERFORM UNTIL INX NOT < 13                                           
134400         MOVE MFS-RENSA-FAELT      TO 4315-MOD-IDRADNR-FOM (INX)          
134500                                      4315-MOD-IDRADNR-TOM (INX)          
134600                                      4315-MOD-KVLEVART (INX)             
134700         ADD +1 TO INX                                                    
134800     END-PERFORM                                                          
134900     .                                                                    
135000     EJECT                                                                
135100 HC-SAMMA-BILD        SECTION.                                            
135200     SKIP3                                                                
135300     MOVE +1 TO BILD-RAD                                                  
135400     PERFORM UNTIL BILD-RAD NOT < MAX-ANT-RADER-PLUS-1                    
135500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDRADNR (BILD-RAD)                
135600                                    MOD-ADLAGOMR (BILD-RAD)               
135700                                    MOD-FLNOLLJ (BILD-RAD)                
135800                                    MOD-KVORAPP (BILD-RAD)                
135900         ADD +1 TO BILD-RAD                                               
136000     END-PERFORM                                                          
136100     .                                                                    
136200     EJECT                                                                
136300 S01-HAEMTA-PLKLST-EJ-KLAR SECTION.                                       
136400                                                                          
136500     MOVE JA                       TO WS-IDPLKLST-SPAR-AR-HOGST           
136600                                      WS-IDPLKLST-NAESTA-AR-HOGST         
136700     MOVE WS-IDDISTR-NUM           TO W-4A1-IDDISTR                       
136800     MOVE WS-IDKUNDNR-NUM          TO W-4A1-IDKUNDNR                      
136900     MOVE WS-IDORDNR               TO W-4A1-IDORDNR                       
137000     PERFORM IMS-GU-KUNDORDER-SEK                                         
137100                                                                          
137200     PERFORM UNTIL KUNDORDER-SEK-SAKNAS                                   
137300                                                                          
137400       MOVE KORD-IDPRODNR              TO WS-JFR-IDPRODNR                 
137500       MOVE KORD-IDUSER                TO WS-JFR-IDANSTNR                 
137600       IF WS-IDPRODNR = WS-JFR-IDPRODNR AND                               
137700          WS-IDANSTNR = WS-JFR-IDANSTNR-5                                 
137800          MOVE 'J'                     TO WS-TRAEFF-PACKARE               
137900                                                                          
138000         IF KORD-KDPAKOLL = 0 OR 1                                        
138100                                                                          
138200          IF ( (KORD-KVORDRAD-PACK  < KORD-KVORDRAD)       OR             
138300               (KORD-KVORDRAD-LEVPL > 0                    AND            
138400                KORD-KVORDRAD-PACK  < KORD-KVORDRAD-LEVPL) )              
138500                                                                          
138600             ADD +1 TO WS-ANT-ODEL-KVAR-ATT-BEHANDLA                      
138700             PERFORM S01A-SPARA-UNDAN-PLOCKLISTNR                         
138800             PERFORM S01B-EV-VISAS-HOGSTA-PLKLST                          
138900          END-IF                                                          
139000         END-IF                                                           
139100       END-IF                                                             
139200       PERFORM IMS-GN-KUNDORDER-SEK                                       
139300     END-PERFORM                                                          
139400                                                                          
139500     PERFORM S01C-KOLL-OM-SISTA-PLOCKLISTA                                
139600     PERFORM S01D-EV-INGA-ODEL-KVAR-ATT-BEH                               
139700     .                                                                    
139800     EJECT                                                                
139900 S01A-SPARA-UNDAN-PLOCKLISTNR            SECTION.                         
140000                                                                          
140100     IF KORD-IDPLKLST    > WS-IDPLKLST-FOM  AND                           
140200        WS-IDPLKLST-SPAR = 0                                              
140300        MOVE KORD-IDPLKLST             TO WS-IDPLKLST-SPAR                
140400     END-IF                                                               
140500                                                                          
140600     IF (MID-FLSVAR         = JA OR YES)        AND                       
140700         KORD-IDPLKLST NOT  = WS-IDPLKLST-SPAR  AND                       
140800         WS-IDPLKLST-NAESTA = 0                                           
140900       MOVE KORD-IDPLKLST              TO WS-IDPLKLST-NAESTA              
141000     END-IF                                                               
141100     .                                                                    
141200     EJECT                                                                
141300 S01B-EV-VISAS-HOGSTA-PLKLST             SECTION.                         
141400                                                                          
141500     IF WS-IDPLKLST-SPAR > 0                    AND                       
141600        WS-IDPLKLST-SPAR NOT = KORD-IDPLKLST                              
141700       MOVE NEJ                    TO WS-IDPLKLST-SPAR-AR-HOGST           
141800     END-IF                                                               
141900                                                                          
142000     IF WS-IDPLKLST-NAESTA > 0                  AND                       
142100        WS-IDPLKLST-NAESTA NOT = KORD-IDPLKLST                            
142200       MOVE NEJ                    TO WS-IDPLKLST-NAESTA-AR-HOGST         
142300     END-IF                                                               
142400     .                                                                    
142500     EJECT                                                                
142600 S01C-KOLL-OM-SISTA-PLOCKLISTA SECTION.                                   
142700                                                                          
142800     IF WS-IDPLKLST-SPAR = ZERO   AND                                     
142900        WS-ANT-ODEL-KVAR-ATT-BEHANDLA > 0                                 
143000       MOVE MID-IDPLKLST-SPAR TO WS-IDPLKLST-SPAR                         
143100                                                                          
143200       IF WS-IDPLKLST-SPAR = WS-IDPLKLST-NAESTA                           
143300         MOVE ZERO TO WS-IDPLKLST-NAESTA                                  
143400       END-IF                                                             
143500     END-IF                                                               
143600     .                                                                    
143700     EJECT                                                                
143800 S01D-EV-INGA-ODEL-KVAR-ATT-BEH          SECTION.                         
143900                                                                          
144000     IF WS-TRAEFF-PACKARE = NEJ                                           
144100*-------------------------------------------SAKNAS ANGIVEN                
144200*-------------------------------------------PACKARE PÅ ORDERN             
144300        MOVE MFS-RENSA-FAELT           TO   MOD-IDDISTR-UT                
144400                                            MOD-IDKUNDNR-UT               
144500                                            MOD-IDORDNR-UT                
144600        MOVE FEL                       TO   WS-INDATA-TEST                
144700        MOVE FEL-719 (INDX)            TO   MOD-TEMFSFEL                  
144800        MOVE MFS-RENSA-FAELT           TO   MOD-FLSVAR                    
144900                                            MOD-FLAGGA                    
145000        MOVE MFS-STAENG-FAELT          TO   MOD-FLSVAR-ATTR               
145100                                            MOD-FLAGGA-ATTR               
145200     ELSE                                                                 
145300        IF WS-ANT-ODEL-KVAR-ATT-BEHANDLA = 0                              
145400*----------------------------------------ÄR ANGIVEN PACKARES              
145500*----------------------------------------ORDERDELAR REDAN KLARA           
145600          MOVE FEL                     TO   WS-INDATA-TEST                
145700          MOVE FEL-720 (INDX)          TO   MOD-TEMFSFEL                  
145800          MOVE MFS-RENSA-FAELT         TO   MOD-FLSVAR                    
145900                                            MOD-FLAGGA                    
146000          MOVE MFS-STAENG-FAELT        TO   MOD-FLSVAR-ATTR               
146100                                            MOD-FLAGGA-ATTR               
146200        END-IF                                                            
146300     END-IF                                                               
146400     .                                                                    
146500     EJECT                                                                
146600 S02-LAGG-UT-RADER      SECTION.                                          
146700     SKIP3                                                                
146800     IF MID-IDPLKLST-SPAR NOT = ZERO     AND                              
146900        MID-IDPLKLST-SPAR NOT = WS-IDPLKLST-SPAR                          
147000        IF WS-4397-STARTAD                                                
147100            CONTINUE                                                      
147200         ELSE                                                             
147300            PERFORM S02A-AATERST-TIDIGARE-PLKLST                          
147400        END-IF                                                            
147500     END-IF                                                               
147600                                                                          
147700     MOVE NEJ                           TO WS-SLINGA-KLAR                 
147800                                                                          
147900     MOVE +1                            TO   RAD-INX                      
148000                                             INX                          
148100                                                                          
148200     MOVE WS-IDDISTR-NUM                TO W-401-IDDISTR                  
148300     MOVE WS-IDKUNDNR-NUM               TO W-401-IDKUNDNR                 
148400     MOVE WS-IDORDNR                    TO W-401-IDORDNR                  
148500     MOVE WS-IDPRODNR                   TO W-401-IDPRODNR                 
148600     MOVE WS-IDPLKLST-SPAR              TO W-401-IDPLKLST                 
148700                                                                          
148800     IF WS-IDPLKLST-SPAR = ZERO                                           
148900       MOVE +001            TO W-401-IDPLKLST                             
149000     END-IF                                                               
149100                                                                          
149200     PERFORM IMS-GHU-KUNDORDER                                            
149300                                                                          
149400     IF KORD-KDPAKOLL = 2                                                 
149500        MOVE FEL                   TO   WS-INDATA-TEST                    
149600        MOVE FEL-804 (INDX)        TO   MOD-TEMFSFEL                      
149700        MOVE MFS-STAENG-FAELT      TO   MOD-FLSVAR-ATTR                   
149800                                        MOD-FLAGGA-ATTR                   
149900     ELSE                                                                 
150000       IF KORD-KDPAKOLL = ZERO                                            
150100          PERFORM S02B-KONTROLL                                           
150200       END-IF                                                             
150300     END-IF                                                               
150400                                                                          
150500     IF WS-INDATA-RATT                                                    
150600       MOVE KORD-IDPLKLST           TO MOD-IDPLKLST                       
150700                                       WS-IDPLKLST                        
150800       INSPECT MOD-IDPLKLST REPLACING LEADING ZERO BY SPACE               
150900       IF WS-IDRADNR-SPAR > ZERO                                          
151000          MOVE WS-IDRADNR-SPAR      TO W-411-IDPURAD                      
151100          PERFORM IMS-GNP-RAD-KVAL                                        
151200       ELSE                                                               
151300          PERFORM IMS-GNP-RAD-OKVAL                                       
151400       END-IF                                                             
151500                                                                          
151600       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
151700                     RAD-INX NOT < MAX-ANT-RADER-PLUS-1                   
151800         MOVE ORAD-IDPURAD                 TO WS-IDRADNR-SPAR             
151900                                                                          
152000         IF ORAD-KDRADSTA < 4                                             
152100            PERFORM S02C-BEHANDLA-EJ-PACKADE-RADER                        
152200            PERFORM S02D-FLYTTA-RADER-TILL-MOD                            
152300            ADD +1              TO RAD-INX                                
152400         END-IF                                                           
152500                                                                          
152600         ADD +1                 TO W-411-IDPURAD                          
152700                                                                          
152800         PERFORM IMS-GNP-RAD-OKVAL                                        
152900       END-PERFORM                                                        
153000                                                                          
153100       PERFORM S02G-KOLL-OAVSLUTADE-KOLLI                                 
153200                                                                          
153300       PERFORM S02E-LAEGG-UT-MEDDELANDEN                                  
153400       PERFORM S02F-RENSA-EJ-IFYLLDA-RADER                                
153500                                                                          
153600       IF ORAD-FINNS                                                      
153700          MOVE WS-IDRADNR-SPAR           TO MOD-IDRADNR-SPAR              
153800       ELSE                                                               
153900          MOVE ZERO                      TO MOD-IDRADNR-SPAR              
154000       END-IF                                                             
154100                                                                          
154200       MOVE MID-IDTRANS-START            TO MOD-IDTRANS-START             
154300     END-IF                                                               
154400     .                                                                    
154500     EJECT                                                                
154600 S02A-AATERST-TIDIGARE-PLKLST SECTION.                                    
154700     SKIP3                                                                
154800     MOVE WS-IDDISTR-NUM    TO W-401-IDDISTR                              
154900     MOVE WS-IDKUNDNR-NUM   TO W-401-IDKUNDNR                             
155000     MOVE WS-IDORDNR        TO W-401-IDORDNR                              
155100     MOVE WS-IDPRODNR       TO W-401-IDPRODNR                             
155200     MOVE MID-IDPLKLST-SPAR TO W-401-IDPLKLST                             
155300     PERFORM IMS-GHU-KUNDORDER-GODK-GE                                    
155400                                                                          
155500     IF SEGMENT-FINNS                                                     
155600       MOVE +0         TO KORD-KDPAKOLL                                   
155700       MOVE NEJ        TO KORD-FLPAFEL                                    
155800       PERFORM IMS-REPL-KUNDORDER                                         
155900     END-IF                                                               
156000     .                                                                    
156100     EJECT                                                                
156200 S02B-KONTROLL SECTION.                                                   
156300     SKIP3                                                                
156400     EVALUATE TRUE                                                        
156500     WHEN  WS-SAMMA-BILD     AND                                          
156600           FL-NYA-NYCKLAR    AND                                          
156700          (MID-FLSVAR = JA   OR  YES)                                     
156800           MOVE FEL            TO   WS-INDATA-TEST                        
156900           MOVE FEL-787 (INDX) TO   MOD-TEMFSFEL                          
157000     WHEN  WS-SAMMA-BILD     AND                                          
157100           FL-GAMLA-NYCKLAR  AND                                          
157200           WS-IDRADNR-SPAR NOT = ZERO                                     
157300           MOVE FEL            TO  WS-INDATA-TEST                         
157400           MOVE FEL-8172(INDX) TO  MOD-TEMFSFEL                           
157500     WHEN OTHER                                                           
157600           MOVE KORD-FLPAFEL   TO   WS-FLPAFEL                            
157700           MOVE 1              TO   KORD-KDPAKOLL                         
157800           MOVE NEJ            TO   KORD-FLPAFEL                          
157900           PERFORM IMS-REPL-KUNDORDER                                     
158000     END-EVALUATE                                                         
158100     .                                                                    
158200     EJECT                                                                
158300 S02C-BEHANDLA-EJ-PACKADE-RADER SECTION.                                  
158400                                                                          
158500     COMPUTE WS-KVORAPP = ORAD-KVAVBART - ORAD-KVLEVART                   
158600     MOVE ORAD-FLNOLLJ              TO WS-FLNOLLJ                         
158700     MOVE ORAD-ADLAGOMR          TO WS-PACKN-OMRADE                       
158800                                                                          
158900     EVALUATE TRUE                                                        
159000     WHEN DCS-CDC OR DCS-CDC-TR                                           
159100       IF WS-NOLLNING-PACK-OMR OR                                         
159200          ORAD-KVLEVART > ZERO OR                                         
159300          WS-KDORDKL    < 3                                               
159400          CONTINUE                                                        
159500***    << INGET NOLLJAGNINGSKRAV OM OVANSTÅNDE VILLKOR ÄR                 
159600***       UPPFYLLT.                                                       
159700       ELSE                                                               
159800          IF WS-FLNOLLJ    = NEJ AND                                      
159900             ORAD-KVLEVART = ZERO                                         
160000             MOVE JA    TO   WS-FLPAFEL                                   
160100             MOVE MFS-ADD-LYS-UPP-FAELT                                   
160200                        TO MOD-IDRADNR-ATTR (RAD-INX)                     
160300          ELSE                                                            
160400             MOVE MFS-FORMATETS-ATTR                                      
160500                        TO MOD-IDRADNR-ATTR (RAD-INX)                     
160600          END-IF                                                          
160700       END-IF                                                             
160800     WHEN DCS-SDC                                                         
160900       IF ORAD-KVLEVART > ZERO OR                                         
161000          WS-KDORDKL    < 3                                               
161100          CONTINUE                                                        
161200***    << INGET NOLLJAGNINGSKRAV OM OVANSTÅNDE VILLKOR ÄR                 
161300***       UPPFYLLT.                                                       
161400       ELSE                                                               
161500          IF WS-FLNOLLJ    = NEJ AND                                      
161600             ORAD-KVLEVART = ZERO                                         
161700             MOVE JA    TO   WS-FLPAFEL                                   
161800             MOVE MFS-ADD-LYS-UPP-FAELT                                   
161900                        TO MOD-IDRADNR-ATTR (RAD-INX)                     
162000          ELSE                                                            
162100             MOVE MFS-FORMATETS-ATTR                                      
162200                        TO MOD-IDRADNR-ATTR (RAD-INX)                     
162300          END-IF                                                          
162400       END-IF                                                             
162500     WHEN DCS-NDC-PF                                                      
162600       IF ORAD-KVLEVART > ZERO                                            
162700                                                                          
162800          CONTINUE                                                        
162900***    << INGET NOLLJAGNINGSKRAV OM OVANSTÅNDE VILLKOR ÄR                 
163000***       UPPFYLLT. NDC NOLLJAGNING SAMTLIGA ORDERKLASSER                 
163100       ELSE                                                               
163200          IF WS-FLNOLLJ    = NEJ AND                                      
163300             ORAD-KVLEVART = ZERO                                         
163700            IF NDC-JP                                                     
163701              CONTINUE                                                    
163702            ELSE                                                          
163703              MOVE JA   TO   WS-FLPAFEL                                   
163704              MOVE MFS-ADD-LYS-UPP-FAELT                                  
163705                         TO MOD-IDRADNR-ATTR (RAD-INX)                    
163706            END-IF                                                        
163800          ELSE                                                            
163900             MOVE MFS-FORMATETS-ATTR                                      
164000                        TO MOD-IDRADNR-ATTR (RAD-INX)                     
164100          END-IF                                                          
164200       END-IF                                                             
164300     WHEN DCS-NDC-NA                                                      
164400       IF ORAD-KVLEVART > ZERO                                            
164500          CONTINUE                                                        
164600***    << INGET NOLLJAGNINGSKRAV OM OVANSTÅNDE VILLKOR ÄR                 
164700***       UPPFYLLT. NDC NOLLJAGNING SAMTLIGA ORDERKLASSER                 
164800       ELSE                                                               
164900          IF WS-FLNOLLJ    = NEJ AND                                      
165000             ORAD-KVLEVART = ZERO                                         
165100             MOVE JA    TO   WS-FLPAFEL                                   
165200             MOVE MFS-ADD-LYS-UPP-FAELT                                   
165300                        TO MOD-IDRADNR-ATTR (RAD-INX)                     
165400          ELSE                                                            
165500             MOVE MFS-FORMATETS-ATTR                                      
165600                        TO MOD-IDRADNR-ATTR (RAD-INX)                     
165700          END-IF                                                          
165800       END-IF                                                             
165900     WHEN OTHER                                                           
166000       CONTINUE                                                           
166100     END-EVALUATE                                                         
166200     .                                                                    
166300     EJECT                                                                
166400 S02D-FLYTTA-RADER-TILL-MOD SECTION.                                      
166500     SKIP3                                                                
166600     MOVE ORAD-IDPURAD   TO WS-IDRADNR-RED                                
166700     MOVE WS-IDRADNR-RED TO MOD-IDRADNR (RAD-INX)                         
166800     MOVE WS-KVORAPP     TO WS-KVORAPP-RED                                
166900     MOVE WS-KVORAPP-RED TO MOD-KVORAPP (RAD-INX)                         
167000     MOVE ORAD-ADLAGOMR  TO WS-ADLAGOMR-N                                 
167100     MOVE WS-ADLAGOMR    TO MOD-ADLAGOMR (RAD-INX)                        
167200     MOVE WS-FLNOLLJ     TO MOD-FLNOLLJ (RAD-INX)                         
167300     .                                                                    
167400     EJECT                                                                
167500 S02E-LAEGG-UT-MEDDELANDEN SECTION.                                       
167600                                                                          
167700     IF WS-FLPAFEL = JA                                                   
167800        MOVE FEL-833 (INDX)        TO MOD-TEMFSFEL                        
167900        PERFORM S02EA-UPPDATERA-FLPAFEL                                   
168000     ELSE                                                                 
168100       IF KOLLIFEL                                                        
168200          MOVE FEL-187 (INDX)      TO MOD-TEMFSFEL                        
168300          PERFORM S02EA-UPPDATERA-FLPAFEL                                 
168400       END-IF                                                             
168500     END-IF                                                               
168600                                                                          
168700     PERFORM S02EB-KONTROLL-FLER-ORDERDELAR                               
168800     PERFORM S02EC-SATT-MEDDELANDE-TEXT                                   
168900     .                                                                    
169000     EJECT                                                                
169100 S02EA-UPPDATERA-FLPAFEL                 SECTION.                         
169200                                                                          
169300     MOVE WS-IDDISTR-NUM      TO W-401-IDDISTR                            
169400     MOVE WS-IDKUNDNR-NUM     TO W-401-IDKUNDNR                           
169500     MOVE WS-IDORDNR          TO W-401-IDORDNR                            
169600     MOVE WS-IDPRODNR         TO W-401-IDPRODNR                           
169700     MOVE WS-IDPLKLST-SPAR    TO W-401-IDPLKLST                           
169800                                                                          
169900     IF WS-IDPLKLST-SPAR = ZERO                                           
170000       MOVE WS-IDPLKLST       TO W-401-IDPLKLST                           
170100     END-IF                                                               
170200     PERFORM IMS-GHU-KUNDORDER                                            
170300                                                                          
170400     MOVE JA                  TO KORD-FLPAFEL                             
170500     PERFORM IMS-REPL-KUNDORDER                                           
170600     .                                                                    
170700     EJECT                                                                
170800 S02EB-KONTROLL-FLER-ORDERDELAR SECTION.                                  
170900     SKIP3                                                                
171000     MOVE NEJ                        TO WS-FLER-ORDERDELAR-FINNS          
171100                                                                          
171200     IF WS-ANT-ODEL-KVAR-ATT-BEHANDLA > 1  AND                            
171300        WS-IDPLKLST-SPAR NOT          = WS-IDPLKLST-NAESTA                
171400        MOVE JA                      TO WS-FLER-ORDERDELAR-FINNS          
171500     ELSE                                                                 
171600        IF WS-ANT-ODEL-KVAR-ATT-BEHANDLA > 2                              
171700           MOVE JA                   TO WS-FLER-ORDERDELAR-FINNS          
171800        END-IF                                                            
171900     END-IF                                                               
172000     .                                                                    
172100     EJECT                                                                
172200 S02EC-SATT-MEDDELANDE-TEXT              SECTION.                         
172300                                                                          
172400     IF RAD-INX NOT < MAX-ANT-RADER-PLUS-1                                
172500       MOVE UPPL-2 (INDX)         TO  MOD-TEMFSINF                        
172600     ELSE                                                                 
172700       IF FLER-ORDERDELAR-FINNS                                           
172800         IF WS-IDPLKLST-SPAR = WS-IDPLKLST-NAESTA                         
172900           IF WS-IDPLKLST-NAESTA-AR-HOGST = JA                            
173000             MOVE UPPL-6 (INDX)            TO  MOD-TEMFSINF               
173100           ELSE                                                           
173200             MOVE UPPL-4 (INDX)            TO  MOD-TEMFSINF               
173300           END-IF                                                         
173400         ELSE                                                             
173500           IF WS-IDPLKLST-SPAR-AR-HOGST = JA                              
173600             MOVE UPPL-6 (INDX)            TO  MOD-TEMFSINF               
173700           ELSE                                                           
173800             MOVE UPPL-4 (INDX)            TO  MOD-TEMFSINF               
173900           END-IF                                                         
174000         END-IF                                                           
174100       ELSE                                                               
174200         MOVE UPPL-5 (INDX)                TO  MOD-TEMFSINF               
174300       END-IF                                                             
174400     END-IF                                                               
174500     .                                                                    
174600     EJECT                                                                
174700 S02F-RENSA-EJ-IFYLLDA-RADER SECTION.                                     
174800     SKIP3                                                                
174900     PERFORM UNTIL RAD-INX NOT < MAX-ANT-RADER-PLUS-1                     
175000         MOVE MFS-RENSA-FAELT     TO MOD-IDRADNR (RAD-INX)                
175100                                     MOD-ADLAGOMR (RAD-INX)               
175200                                     MOD-KVORAPP (RAD-INX)                
175300                                     MOD-FLNOLLJ (RAD-INX)                
175400         ADD +1 TO RAD-INX                                                
175500     END-PERFORM                                                          
175600     .                                                                    
175700     EJECT                                                                
175800 S02G-KOLL-OAVSLUTADE-KOLLI SECTION.                                      
175900                                                                          
176000     MOVE NEJ TO WS-KOLLIFEL                                              
176100     MOVE WS-IDPRODNR TO W-4301-IDPRODNR                                  
176200     PERFORM IMS-GU-XXDU01                                                
176300     IF SEGMENT-FINNS                                                     
176400        PERFORM IMS-GNP-XXDU11                                            
176500        PERFORM UNTIL SEGMENT-SAKNAS OR KOLLIFEL                          
176600          IF 4302-IDPLKLST = WS-IDPLKLST-SPAR   AND                       
176700             4302-KDKOLSTA = 0                  AND                       
176800             4302-FLBANDST = NEJ                                          
176900             MOVE JA TO WS-KOLLIFEL                                       
177000          ELSE                                                            
177100             PERFORM IMS-GNP-XXDU11                                       
177200          END-IF                                                          
177300        END-PERFORM                                                       
177400     END-IF                                                               
177500     .                                                                    
177600     SKIP2                                                                
177700* MFS SEKTIONER                                                           
177800     SKIP3                                                                
177900 MFS-RENSA-FAELT-UT SECTION.                                              
178000     SKIP2                                                                
178100     MOVE MFS-RENSA-FAELT TO MOD-IDANSTNR-UT                              
178200                             MOD-IDDISTR-UT                               
178300                             MOD-IDKUNDNR-UT                              
178400                             MOD-IDORDNR-UT                               
178500                             MOD-IDKOLLI-UT                               
178600                             MOD-IDPRODNR-UT                              
178700                             MOD-IDDC-UT                                  
178800                             MOD-IDPLKLST                                 
178900     .                                                                    
179000     SKIP3                                                                
179100 MFS-RENSA-FAELT-IN SECTION.                                              
179200     SKIP2                                                                
179300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
179400                             MOD-IDANSTNR-IN                              
179500                             MOD-IDDISTR-IN                               
179600                             MOD-IDKUNDNR-IN                              
179700                             MOD-IDORDNR-IN                               
179800                             MOD-IDKOLLI-IN                               
179900                             MOD-IDPRODNR-IN                              
180000                             MOD-IDDC-IN                                  
180100                             MOD-ADFLGEO                                  
180200                             MOD-ADFLOMR                                  
180300                             MOD-ADRUTNIV                                 
180400                             MOD-TEMFSINF                                 
180500     .                                                                    
180600     EJECT                                                                
180700* IMS SEKTIONER                                                           
180800     SKIP3                                                                
180900 IMS-GET-MSG SECTION.                                                     
181000                                                                          
181100     MOVE '  QC' TO GODK-STATUSKODER                                      
181200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
181300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
181400     PERFORM IMS-STATUSKONTROLL                                           
181500     .                                                                    
181600     SKIP3                                                                
181700 IMS-INSERT-MSG SECTION.                                                  
181800                                                                          
181900     IF ENGLISH-TEXT                                                      
182000       MOVE 'N' TO MFS-KDHUVOMR                                           
182100     END-IF                                                               
182200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
182300     MOVE SPACE TO GODK-STATUSKODER                                       
182400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
182500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
182600     PERFORM IMS-STATUSKONTROLL                                           
182700     .                                                                    
182800     EJECT                                                                
182900 IMS-INSERT-ALTMSG SECTION.                                               
183000                                                                          
183100     MOVE LOW-VALUE TO ALT-Z1 ALT-Z2                                      
183200     MOVE SPACE TO GODK-STATUSKODER                                       
183300     CALL CBLTDLI USING ISRT ALT-PCB ALT-IO-AREA                          
183400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
183500     PERFORM IMS-STATUSKONTROLL                                           
183600     .                                                                    
183700     SKIP3                                                                
183800 IMS-GU-KUNDORDER-SEK SECTION.                                            
183900     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
184000            DELIMITED BY SIZE INTO SSA1                                   
184100     MOVE '  GE' TO GODK-STATUSKODER                                      
184200     CALL CBLTDLI USING GU     WDE42-PCB DLI-IO-AREA SSA1                 
184300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
184400                               STATUS-KUNDORDER-SEK-WS                    
184500     PERFORM IMS-STATUSKONTROLL                                           
184600     .                                                                    
184700     SKIP2                                                                
184800 IMS-GN-KUNDORDER-SEK SECTION.                                            
184900     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
185000            DELIMITED BY SIZE INTO SSA1                                   
185100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
185200     CALL CBLTDLI USING GN     WDE42-PCB DLI-IO-AREA SSA1                 
185300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
185400                               STATUS-KUNDORDER-SEK-WS                    
185500     PERFORM IMS-STATUSKONTROLL                                           
185600     .                                                                    
185700     EJECT                                                                
185800 IMS-GU-KUNDORDER SECTION.                                                
185900     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
186000            DELIMITED BY SIZE INTO SSA1                                   
186100     MOVE '    ' TO GODK-STATUSKODER                                      
186200     CALL CBLTDLI USING GU     WDE4-PCB DLI-IO-AREA SSA1                  
186300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
186400     PERFORM IMS-STATUSKONTROLL                                           
186500     .                                                                    
186600     SKIP2                                                                
186700 IMS-GHU-KUNDORDER SECTION.                                               
186800     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
186900            DELIMITED BY SIZE INTO SSA1                                   
187000     MOVE '    ' TO GODK-STATUSKODER                                      
187100     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-AREA SSA1                  
187200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
187300     PERFORM IMS-STATUSKONTROLL                                           
187400     .                                                                    
187500     SKIP2                                                                
187600 IMS-GHU-KUNDORDER-GODK-GE SECTION.                                       
187700     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
187800            DELIMITED BY SIZE INTO SSA1                                   
187900     MOVE 'GE  ' TO GODK-STATUSKODER                                      
188000     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-AREA SSA1                  
188100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
188200     PERFORM IMS-STATUSKONTROLL                                           
188300     .                                                                    
188400     SKIP2                                                                
188500 IMS-GNP-RAD-KVAL SECTION.                                                
188600     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
188700            DELIMITED BY SIZE INTO SSA1                                   
188800     STRING 'WDE411  (IDPURAD  >' W-WDE411-IDPURAD-X ')'                  
188900            DELIMITED BY SIZE INTO SSA2                                   
189000     MOVE '  GE' TO GODK-STATUSKODER                                      
189100     CALL CBLTDLI USING GNP    WDE4-PCB DLI-IO-AREA SSA1 SSA2             
189200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
189300                              STATUS-ORAD-WS                              
189400     PERFORM IMS-STATUSKONTROLL                                           
189500     .                                                                    
189600     SKIP2                                                                
189700 IMS-GNP-RAD-OKVAL SECTION.                                               
189800     MOVE   'WDE411'       TO   SSA1                                      
189900     MOVE '  GE' TO GODK-STATUSKODER                                      
190000     CALL CBLTDLI USING GNP    WDE4-PCB DLI-IO-AREA SSA1                  
190100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
190200                              STATUS-ORAD-WS                              
190300     PERFORM IMS-STATUSKONTROLL                                           
190400     .                                                                    
190500     SKIP2                                                                
190600 IMS-REPL-KUNDORDER     SECTION.                                          
190700     MOVE '  ' TO GODK-STATUSKODER                                        
190800     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-AREA                         
190900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
191000     PERFORM IMS-STATUSKONTROLL                                           
191100     .                                                                    
191200     SKIP2                                                                
191300 IMS-GU-IDPRODNR  SECTION.                                                
191400     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
191500            DELIMITED BY SIZE INTO SSA1                                   
191600     MOVE '  GE' TO GODK-STATUSKODER                                      
191700     CALL CBLTDLI USING GU     WDE6-PCB DLI-IO-AREA SSA1                  
191800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
191900     PERFORM IMS-STATUSKONTROLL                                           
192000     .                                                                    
192100     EJECT                                                                
192200 IMS-GU-KUNDORDER-SEK-INV SECTION.                                        
192300     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X ')'                
192400                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
192500            DELIMITED BY SIZE INTO SSA1                                   
192600     MOVE 'WDE401' TO SSA2                                                
192700     MOVE '  GEGB'  TO GODK-STATUSKODER                                   
192800     CALL CBLTDLI USING GU    WDE43-PCB DLI-IO-AREA SSA1 SSA2             
192900     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
193000     PERFORM IMS-STATUSKONTROLL                                           
193100     .                                                                    
193200     EJECT                                                                
193300 IMS-GU-XXDU01      SECTION.                                              
193400                                                                          
193500     STRING 'WLXXDU01(WDGXKEY  =' W-4301-WDGXKEY-X ')'                    
193600            DELIMITED BY SIZE INTO SSA1                                   
193700     MOVE '  GE' TO GODK-STATUSKODER                                      
193800     CALL CBLTDLI USING GU   XXDU-PCB DLI-IO-AREA1 SSA1                   
193900     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
194000     PERFORM IMS-STATUSKONTROLL                                           
194100     .                                                                    
194200     SKIP2                                                                
194300 IMS-GNP-XXDU11       SECTION.                                            
194400                                                                          
194500     MOVE 'WLXXDU11 '  TO SSA1                                            
194600     MOVE '  GE' TO GODK-STATUSKODER                                      
194700     CALL CBLTDLI USING GNP XXDU-PCB DLI-IO-AREA1 SSA1                    
194800     MOVE XXDU-STATUS-CODE TO STATUS-WS                                   
194900     PERFORM IMS-STATUSKONTROLL                                           
195000     .                                                                    
195100                                                                          
195200 IMS-GU-WDB601    SECTION.                                                
195300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
195400          DELIMITED BY SIZE INTO SSA1                                     
195500     MOVE '  ' TO GODK-STATUSKODER                                        
195600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
195700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
195800     PERFORM IMS-STATUSKONTROLL                                           
195900     .                                                                    
196000     EJECT                                                                
196100 IMS-STATUSKONTROLL SECTION.                                              
196200     SET STATUS-IX TO 1                                                   
196300     SEARCH GODK-STATUS AT END CALL FELLOG                                
196400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
196500     END-SEARCH                                                           
196600     .                                                                    
