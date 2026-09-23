000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4034200.                                                
000400 AUTHOR.         CAP GEMINI AB/EP.                                        
000500 DATE-WRITTEN.   JAN  86.                                                 
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION.                                                            
001000*        PROGRAMMET SKRIVER LISTA PACKADE ORDER.                          
001100*                                                                         
001200*    NYCKELALTERNATIV.                                                    
001300*        PRODUKTIONSNUMMER.                                               
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T342U                                             
001700*        MID:         W4I34201                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O34201                                            
002100*        LISTA:       L4O34201                                            
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002701                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 77   IDPGM                      PIC X(8)    VALUE 'W4034200'.            
002900 77    JA                        PIC X       VALUE 'J'.                   
003000 77    NEJ                       PIC X       VALUE 'N'.                   
003100 77    FEL                       PIC X       VALUE 'F'.                   
003200 77    RAETT                     PIC X       VALUE 'R'.                   
003300 77    WS-DUMMY                  PIC X(50)   VALUE SPACE.                 
003400 77    PACK-NORDEN-C1            PIC X(8)    VALUE '031     '.            
003410*R44059                                                                   
003500 77    PACK-EXP-C1               PIC X(8)    VALUE '032     '.            
003510*R44059                                                                   
003600 77    PACK-C2                   PIC X(8)    VALUE '033     '.            
003610*R3130462   MAASTRICHT                                                    
003700 77    WS-LISTA                  PIC X(8)    VALUE SPACE.                 
003800 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
003900 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +118 COMP SYNC.        
004000 77    WS-IDPRODNR               PIC X(7)    VALUE SPACE.                 
005000 77    ACK-ANTAL-RADER           PIC 9(3)    VALUE ZERO.                  
005100 77    ACK-ANTAL-SIDOR           PIC 9(3)    VALUE ZERO.                  
005200     SKIP2                                                                
005300 77    WS-IDTRANS                PIC X(04).                               
005400   88  WS-SAMMA-BILD                        VALUE '4342'.                 
005500   88  WS-GODKAND-BILD                      VALUE '433A' '433E'           
005600                                                  '433G' '4341'           
005700                                                  '439G' '439H'           
005710                                                  '43AV'.                 
005800     SKIP2                                                                
005900 77    WS-INDATA-TEST            PIC X(01).                               
006000   88  WS-INDATA-FEL                        VALUE 'F'.                    
006100   88  WS-INDATA-RATT                       VALUE 'R'.                    
006200     SKIP2                                                                
006300 01    WS-IDKUNDRF.                                                       
006400   03    WS-IDORDNR              PIC 9(5).                                
006500   03    FILLER                  PIC X(5).                                
006600     EJECT                                                                
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800   03  W006PRS1                  PIC X(8)  VALUE 'W006PRS1'.              
006900   03  CBLTDLI                   PIC X(8)  VALUE 'CBLTDLI '.              
007000   03  FELLOG                    PIC X(8)  VALUE 'FELLOG  '.              
007100     EJECT                                                                
007200 01    NYCKLAR-TILL-DLI.                                                  
007210*                                                                         
007220   03    W-WDE4F1KY-MAX-X.                                                
007230     05    W-IDPRODNR-WDE4F-MAX  PIC S9(7)   VALUE ZERO  COMP-3.          
007240     05    W-WDE4F1-MAX          PIC X(25)   VALUE HIGH-VALUE.            
007250                                                                          
007260   03    W-WDE4F1KY-MIN-X.                                                
007270     05    W-IDPRODNR-WDE4F-MIN  PIC S9(7)   VALUE ZERO  COMP-3.          
007280     05    W-WDE4F1-MIN          PIC X(25)   VALUE LOW-VALUE.             
007300*                                                                         
007400   03    W-WDE601-IDPRODNR-X.                                             
007500     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
007600*                                                                         
007700   03    W-WDE611-IDKOLLI-X.                                              
007800     05    W-610-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
007810*                                                                         
007811     03  W-WDQ301KY-MIN-X.                                                
007812         05  W-Q301-MIN-IDORDER  PIC S9(7) COMP-3.                        
007813         05  W-Q301-MIN-IDDC     PIC X(2).                                
007814         05  W-Q301-MIN-IDPRODNR PIC S9(7) COMP-3.                        
007815         05  W-Q301-MIN-IDPLKLST PIC S9(3) COMP-3.                        
007816                                                                          
007817     03  W-WDQ301KY-MAX-X.                                                
007818         05  W-Q301-MAX-IDORDER  PIC S9(7) COMP-3.                        
007819         05  W-Q301-MAX-IDDC     PIC X(2).                                
007820         05  W-Q301-MAX-IDPRODNR PIC S9(7) COMP-3.                        
007821         05  W-Q301-MAX-IDPLKLST PIC S9(3) COMP-3.                        
007830*                                                                         
007840     03  W-IDDC-B6-X.                                                     
007850         05 W-IDDC-B6                  PIC X(2).                          
007900     SKIP3                                                                
008000 01  TEST-IDDISTR                PIC 9(5)                COMP-3.          
008100*01  FILLER -COPY WWDIST48       -RED TEST-IDDISTR.                       
008200     EJECT                                                                
008300 01    MEDDELANDE.                                                        
008400*                                                                         
008500   03    MEDD-1.                                                          
008600     05  FILLER                  PIC X(40)   VALUE                        
008700        'MER KOLLIN FINNS                        '.                       
008800     05  FILLER                  PIC X(40)   VALUE                        
008900        '                                        '.                       
009000   03    FILLER  REDEFINES  MEDD-1.                                       
009100     05  MEDDELANDE-1 OCCURS 2   PIC X(40).                               
009200     SKIP2                                                                
009300   03    FEL-1.                                                           
009400     05  FILLER                  PIC X(40)   VALUE                        
009500        '749 FEL NYCKEL                          '.                       
009600     05  FILLER                  PIC X(40)   VALUE                        
009700        '749 WRONG KEY                           '.                       
009800   03    FILLER  REDEFINES  FEL-1.                                        
009900     05  FEL-749     OCCURS 2    PIC X(40).                               
010000     EJECT                                                                
010010************************************************************              
010100 01    TEXT-RADER.                                                        
010200*                                                                         
010300   03    RUBRIKRAD-1-SVE.                                                 
010310     05  FILLER                  PIC X(31)   VALUE                        
010320        'AB VOLVO CAR CUSTOMER SERVICE  '.                                
010600     05  FILLER                  PIC X(20)   VALUE                        
010700        'PACKAD ORDERLISTA   '.                                           
010800     05  RUB-1-DATUM-SVE         PIC 9(06).                               
010900     05  FILLER                  PIC X(08)   VALUE SPACE.                 
011000     05  FILLER                  PIC X(11)   VALUE                        
011100        '       SID '.                                                    
011200     05  RUB-1-SIDNR-SVE         PIC Z(2)9.                               
011300     05  FILLER                  PIC X(50)   VALUE SPACE.                 
011400     SKIP2                                                                
011500   03    RUBRIKRAD-2-SVE.                                                 
011600     05  FILLER                  PIC X(46)   VALUE                        
011700        ' DISTR   KUNDNR   ORDERNR   DC   FK   KLASS   '.                 
011800     05  FILLER                  PIC X(53)   VALUE                        
011900        'PRODNR   BRUTTOVOL   BRUTTOVKT   ANTKLI   FÄRDIG-PACK'.          
012000     05  FILLER                  PIC X(33)   VALUE SPACE.                 
012100     SKIP2                                                                
012110   03    RUBRIKRAD-2-LDC.                                                 
012120     05  FILLER                  PIC X(46)   VALUE                        
012130        ' DISTR   KUNDNR   ORDERNR   DC FK   KLASS '.                     
012140     05  FILLER                  PIC X(53)   VALUE                        
012150        'PRODNR BRUTTOVOL   BRUTTOVKT   ANTKLI'.                          
012160     05  FILLER                  PIC X(33)   VALUE SPACE.                 
012170     SKIP2                                                                
012200   03    RUBRIKRAD-3-SVE.                                                 
012300     05  FILLER                  PIC X(41)   VALUE                        
012400        ' KOLLI  KOLLIKOD  EMBTYP  LÄNGD  BREDD   '.                      
012500     05  FILLER                  PIC X(35)   VALUE                        
012600        'HÖJD  BRUTTOVOL  BRUTTOVKT  PACKDAT'.                            
012700     05  FILLER                  PIC X(37)   VALUE                        
012800        '  GEO  OMR/STÄLL  RUT/NIV  VMOD  FARL'.                          
012900     05  FILLER                  PIC X(05)   VALUE SPACE.                 
013000     SKIP2                                                                
013010   03    RUBRIKRAD-3-LDC.                                                 
013020     05  FILLER                  PIC X(41)   VALUE                        
013030        ' KOLLI  KOLLIKOD  EMBTYP  LÄNGD  BREDD   '.                      
013040     05  FILLER                  PIC X(42)   VALUE                        
013050        'HÖJD  BRUTTOVOL  BRUTTOVKT  PACKDAT FARL'.                       
013080     05  FILLER                  PIC X(37)   VALUE SPACE.                 
013090     SKIP2                                                                
013100   03    FOT1-SVE.                                                        
013200     05  FILLER                  PIC X(08)   VALUE                        
013300        ' FORTS. '.                                                       
013400     05  FILLER                  PIC X(124)  VALUE SPACE.                 
013500     EJECT                                                                
013600*                                                                         
013700   03    RUBRIKRAD-1-ENG.                                                 
013710     05  FILLER                  PIC X(31)   VALUE                        
013720        ' VOLVO CAR CORP. AFTER SALES.  '.                                
014000     05  FILLER                  PIC X(20)   VALUE                        
014100        'LIST: PACKED ORDERS '.                                           
014200     05  RUB-1-DATUM-ENG         PIC 9(06).                               
014300     05  FILLER                  PIC X(28)   VALUE SPACE.                 
014400     05  FILLER                  PIC X(11)   VALUE                        
014500        '      PAGE '.                                                    
014600     05  RUB-1-SIDNR-ENG         PIC Z(2)9.                               
014700     05  FILLER                  PIC X(33)   VALUE SPACE.                 
014800     SKIP2                                                                
014900   03    RUBRIKRAD-2-ENG.                                                 
015000     05  FILLER                  PIC X(46)   VALUE                        
015100        ' DISTR    CUST.     ORDER   DC   FC   CLASS   '.                 
015200     05  FILLER                  PIC X(53)   VALUE                        
015300        'PR.NO.     GR.VOL.   GR.WEIGHT    CASES   PACK. CASES'.          
015400     05  FILLER                  PIC X(33)   VALUE SPACE.                 
015500     SKIP2                                                                
015600   03    RUBRIKRAD-3-ENG.                                                 
015700     05  FILLER                  PIC X(41)   VALUE                        
015800        '  CASE  CASE CD.  P.TYPE    LTH  WIDTH   '.                      
015900     05  FILLER                  PIC X(35)   VALUE                        
016000        'HGTH  GROSS VOL   GR. WGTH  PACKDAY'.                            
016100     05  FILLER                  PIC X(37)   VALUE                        
016200        '  GEO  OMR/STÄLL  RUT/NIV  VMOD  FARL'.                          
016300     05  FILLER                  PIC X(19)   VALUE SPACE.                 
016400     SKIP2                                                                
016500   03    FOT1-ENG.                                                        
016600     05  FILLER                  PIC X(08)   VALUE                        
016700        ' CONT:D '.                                                       
016800     05  FILLER                  PIC X(124)  VALUE SPACE.                 
016900     EJECT                                                                
017000 01    BLANKRAD.                                                          
017100       03 FILLER                 PIC X(132)  VALUE SPACE.                 
017200     SKIP2                                                                
017300 01    FILLER                 PIC X(16) VALUE 'LDC-INFO-RADER'.           
017310 01   WS-LDC-INFO-RAD.                                                    
017320   03 FILLER                PIC X(2) VALUE SPACE.                         
017330   03 WS-INFO-RAD           PIC X(78).                                    
017340   03 FILLER                PIC X(52) VALUE SPACE.                        
017350                                                                          
017360 01    FILLER                 PIC X(16) VALUE 'LIST-DETALJRADER'.         
017400 01    LIST-RADER.                                                        
017500*                                                                         
017600   03    HUVUDRAD.                                                        
017700     05  FILLER                  PIC X(01)   VALUE SPACE.                 
017800     05  RAD1-IDDISTR            PIC Z(4)9.                               
017900     05  FILLER                  PIC X(02)   VALUE SPACE.                 
018000     05  RAD1-IDKUNDNR           PIC Z(7).                                
018100     05  FILLER                  PIC X(04)   VALUE SPACE.                 
018200     05  RAD1-IDORDNR            PIC Z(4)9.                               
018300     05  FILLER                  PIC X(04)   VALUE SPACE.                 
018400     05  RAD1-IDDC               PIC X(2).                                
018500     05  FILLER                  PIC X(04)   VALUE SPACE.                 
018600     05  RAD1-KDFRAKT            PIC 9(2).                                
018700     05  FILLER                  PIC X(05)   VALUE SPACE.                 
018800     05  RAD1-KDORDKL            PIC 9(1).                                
018900     05  FILLER                  PIC X(04)   VALUE SPACE.                 
019000     05  RAD1-IDPRODNR           PIC Z(6)9.                               
019100     05  FILLER                  PIC X(03)   VALUE SPACE.                 
019200     05  RAD1-VLORDBTO           PIC Z(3)9.9(3).                          
019300     05  FILLER                  PIC X(03)   VALUE SPACE.                 
019400     05  RAD1-VKORDBTO           PIC Z(5)9.9.                             
019500     05  FILLER                  PIC X(04)   VALUE SPACE.                 
019600     05  RAD1-KVKOLLI            PIC Z(4)9.                               
019910     05  FILLER                  PIC X(06)   VALUE SPACE.                 
019920     05  RAD1-TIPACKN            PIC 9(6).                                
019930     05  FILLER                  PIC X(36)   VALUE SPACE.                 
020000     SKIP2                                                                
020010   03    HUVUDRAD-LDC.                                                    
020020     05  FILLER                  PIC X(01)   VALUE SPACE.                 
020030     05  LDC-RAD1-IDDISTR        PIC Z(4)9.                               
020040     05  FILLER                  PIC X(02)   VALUE SPACE.                 
020050     05  LDC-RAD1-IDKUNDNR       PIC Z(7).                                
020060     05  FILLER                  PIC X(02)   VALUE SPACE.                 
020070     05  LDC-RAD1-IDORDNR        PIC Z(4)9.                               
020080     05  FILLER                  PIC X(04)   VALUE SPACE.                 
020090     05  LDC-RAD1-IDDC           PIC X(2).                                
020091     05  FILLER                  PIC X(02)   VALUE SPACE.                 
020092     05  LDC-RAD1-KDFRAKT        PIC 9(2).                                
020093     05  FILLER                  PIC X(05)   VALUE SPACE.                 
020094     05  LDC-RAD1-KDORDKL        PIC 9(1).                                
020095     05  FILLER                  PIC X(02)   VALUE SPACE.                 
020096     05  LDC-RAD1-IDPRODNR       PIC Z(6)9.                               
020097     05  FILLER                  PIC X(03)   VALUE SPACE.                 
020098     05  LDC-RAD1-VLORDBTO       PIC Z(3)9.9(3).                          
020099     05  FILLER                  PIC X(03)   VALUE SPACE.                 
020100     05  LDC-RAD1-VKORDBTO       PIC Z(5)9.9.                             
020101     05  FILLER                  PIC X(04)   VALUE SPACE.                 
020102     05  LDC-RAD1-KVKOLLI        PIC Z(4)9.                               
020105     05  FILLER                  PIC X(48)   VALUE SPACE.                 
020106     EJECT                                                                
020110   03    DETALJRAD.                                                       
020200     05  FILLER                  PIC X(01)   VALUE SPACE.                 
020300     05  RAD2-IDKOLLI            PIC Z(4)9.                               
020400     05  FILLER                  PIC X(02)   VALUE SPACE.                 
020500     05  RAD2-KDKOLLI            PIC X(8).                                
020600     05  FILLER                  PIC X(05)   VALUE SPACE.                 
020700     05  RAD2-KDEMBTYP           PIC 9(1).                                
020800     05  FILLER                  PIC X(04)   VALUE SPACE.                 
020900     05  RAD2-DIKOLLIL           PIC Z(4)9.                               
021000     05  FILLER                  PIC X(02)   VALUE SPACE.                 
021100     05  RAD2-DIKOLLIB           PIC Z(3)9.                               
021200     05  FILLER                  PIC X(03)   VALUE SPACE.                 
021300     05  RAD2-DIKOLLIH           PIC Z(3)9.                               
021400     05  FILLER                  PIC X(04)   VALUE SPACE.                 
021500     05  RAD2-VLORDBTO           PIC Z(3)9.9(3).                          
021600     05  FILLER                  PIC X(02)   VALUE SPACE.                 
021700     05  RAD2-VKORDBTO           PIC Z(5)9.9.                             
021800     05  FILLER                  PIC X(03)   VALUE SPACE.                 
021900     05  RAD2-TIPACKN            PIC 9(6).                                
022000     05  FILLER                  PIC X(03)   VALUE SPACE.                 
022100     05  RAD2-ADFLGEO            PIC X(03).                               
022200     05  FILLER                  PIC X(04)   VALUE SPACE.                 
022300     05  RAD2-ADFLOMR            PIC Z(03).                               
022400     05  FILLER                  PIC X(08)   VALUE SPACE.                 
022500     05  RAD2-ADRUTNIV           PIC Z(03).                               
022600     05  FILLER                  PIC X(05)   VALUE SPACE.                 
022700     05  RAD2-ADVMODUL           PIC Z(03).                               
022800     05  FILLER                  PIC X(03)   VALUE SPACE.                 
022900     05  RAD2-FARLIG             PIC X(02).                               
023000     05  FILLER                  PIC X(20)   VALUE SPACE.                 
023100     SKIP2                                                                
023110   03    DETALJRAD-LDC.                                                   
023120     05  FILLER                  PIC X(01)   VALUE SPACE.                 
023130     05  LDC-RAD2-IDKOLLI        PIC Z(4)9.                               
023140     05  FILLER                  PIC X(02)   VALUE SPACE.                 
023150     05  LDC-RAD2-KDKOLLI        PIC X(8).                                
023160     05  FILLER                  PIC X(05)   VALUE SPACE.                 
023170     05  LDC-RAD2-KDEMBTYP       PIC 9(1).                                
023180     05  FILLER                  PIC X(04)   VALUE SPACE.                 
023190     05  LDC-RAD2-DIKOLLIL       PIC Z(4)9.                               
023191     05  FILLER                  PIC X(02)   VALUE SPACE.                 
023192     05  LDC-RAD2-DIKOLLIB       PIC Z(3)9.                               
023193     05  FILLER                  PIC X(03)   VALUE SPACE.                 
023194     05  LDC-RAD2-DIKOLLIH       PIC Z(3)9.                               
023195     05  FILLER                  PIC X(04)   VALUE SPACE.                 
023196     05  LDC-RAD2-VLORDBTO       PIC Z(3)9.9(3).                          
023197     05  FILLER                  PIC X(02)   VALUE SPACE.                 
023198     05  LDC-RAD2-VKORDBTO       PIC Z(5)9.9.                             
023199     05  FILLER                  PIC X(03)   VALUE SPACE.                 
023200     05  LDC-RAD2-TIPACKN        PIC 9(6).                                
023201     05  FILLER                  PIC X(03)   VALUE SPACE.                 
023210     05  LDC-RAD2-FARLIG         PIC X(02).                               
023211     05  FILLER                  PIC X(52)   VALUE SPACE.                 
023220     SKIP3                                                                
023222*BA                                                                       
023223 01  PU-LDC-1A-RAD-ASTERIX.                                               
023224     03 FILLER                       PIC X(05)  VALUE SPACE.              
023225     03 LDC-1A-RAD-ASTERIXDEL1       PIC X(35)  VALUE                     
023226        '***********************************'.                            
023227     03 LDC-1A-RAD-ASTERIXDEL2       PIC X(34)  VALUE                     
023228        '**********************************'.                             
023229     03 FILLER                       PIC X(01)  VALUE SPACE.              
023230                                                                          
023231 01  PU-LDC-1A-RAD1.                                                      
023232     03 FILLER                       PIC X(05)  VALUE SPACE.              
023233     03 LDC-1A-RAD-TEXT-1            PIC X(41)  VALUE                     
023234        'ORDERN INNEHÅLLER DELAR SOM KOMMER DIREKT'.                      
023235     03 LDC-1A-RAD-TEXT-2            PIC X(26)  VALUE                     
023236        'FRÅN LEVERANTÖR ELLER CDC:'.                                     
023237     03 FILLER                       PIC X(09)  VALUE SPACE.              
023238                                                                          
023239 01  PU-LDC-1A-CDC-RAD.                                                   
023240     03 FILLER                       PIC X(10)  VALUE SPACE.              
023241     03 LDC-1A-RAD-CDC-TEXT-1        PIC X(40)  VALUE                     
023242        '*   OBS! PACKA MED ORDER-RADER FRÅN CDC.'.                       
023243     03 LDC-1A-RAD-CDC-TEXT-2        PIC X(29)  VALUE                     
023244        '                            *'.                                  
023245     03 FILLER                       PIC X(01)  VALUE SPACE.              
023246                                                                          
023247 01  PU-LDC-1A-DIR-RAD.                                                   
023248     03 FILLER                       PIC X(10)  VALUE SPACE.              
023249     03 LDC-1A-RAD-DIR-TEXT          PIC X(51)  VALUE                     
023250        '*   OBS! PACKA MED ORDER-RAD FRÅN DIREKT LEV. NR:'.              
023251     03 LDC-1A-RAD-DIR-NR            PIC X(05).                           
023252     03 LDC-1A-RAD-DIR-ASTER         PIC X(11)  VALUE                     
023253        '          *'.                                                    
023254     03 FILLER                       PIC X(01)  VALUE SPACE.              
023255                                                                          
023256 01  PU-LDC-1A-TOM-RAD.                                                   
023257     03 FILLER                       PIC X(10)  VALUE SPACE.              
023258     03 LDC-1A-RAD-TOM-RAD-DEL1      PIC X(35)  VALUE                     
023259        '*                                  '.                            
023260     03 LDC-1A-RAD-TOM-RAD-DEL2      PIC X(34)  VALUE                     
023261        '                                 *'.                             
023262     03 FILLER                       PIC X(01)  VALUE SPACE.              
023263                                                                          
023270     SKIP3                                                                
023300*01  -COPY W006PRAR                                                       
023400     EJECT                                                                
023500******************************************************************        
023600*                                                                *        
023700*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
023800*                                                                *        
023900******************************************************************        
024000 01    FILLER                 PIC X(16) VALUE 'MID W4I34201 MID'.         
024100     SKIP3                                                                
024200*01    MID -COPY W4I34201.                                                
024300     EJECT                                                                
024400*01    -COPY WMSGAREA                                                     
024500     EJECT                                                                
024600*  03    MOD -COPY W4O34201  -RED MSG-AREA.                               
025900     EJECT                                                                
026000 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
026100     SKIP3                                                                
026200*01    -COPY WMFSAREA                                                     
026300     EJECT                                                                
026400******************************************************************        
026500*                                                                         
026600*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026700*                                                                         
026800 01    IMS-WS.                                                            
026900   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
027000     SKIP3                                                                
027100*                        **** STATUS-KOD FRÅN IMS                         
027200   03    STATUS-WS               PIC XX.                                  
027300     88    SEGMENT-FINNS                     VALUE '  '.                  
027400     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
027500     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
027600     SKIP3                                                                
027700   03    GODK-STATUSKODER.                                                
027800     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
027900     SKIP3                                                                
028000 01    SSA1                      PIC X(90).                               
028100 01    SSA2                      PIC X(64).                               
028200 01    SSA3                      PIC X(64).                               
028300 01    SSA4                      PIC X(64).                               
028400     EJECT                                                                
028500*                            IMS FUNKTIONSKODER                           
028600*01    -COPY W0003                                                        
028700     EJECT                                                                
028800*                            DLI INPUT-OUTPUT AREA                        
028900 01    DLI-IO-AREA.                                                       
029000   03    IO-AREA                 PIC X(500)  VALUE SPACE.                 
029100     SKIP3                                                                
029200*  03    WDE601   -COPY WDE601             -RED IO-AREA.                  
029300     EJECT                                                                
029400*  03    WDE611   -COPY WDE611             -RED IO-AREA.                  
029601     SKIP3                                                                
029602 01  WDE4F-AREA   -COPY WDE4F1.                                           
029603                                                                          
029604 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
029605 01   DLI-IO-AREA-B601.                                                   
029606*     03  -COPY WDB601                                                    
029607     EJECT                                                                
029800 LINKAGE SECTION.                                                         
029900*01    -COPY W0009     -PRE MSG-                                          
030000                                                                          
030100*01    -COPY W0009     -PRE ALT-                                          
030200     EJECT                                                                
030300*01    -COPY W0008     -PRE WDE6-                                         
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030510*01    -COPY W0008     -PRE WDE4F-                                        
030520     05  FILLER                  PIC X.                                   
030530*01    -COPY W0008     -PRE WDB6-                                         
030540     05  FILLER                  PIC X.                                   
030700 PROCEDURE DIVISION USING  MSG-PCB  ALT-PCB                               
030701                           WDE6-PCB WDE4F-PCB WDB6-PCB.                   
030702                                                                          
030710 MAIN SECTION.                                                            
030800     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB                               
030810                           WDE6-PCB WDE4F-PCB WDB6-PCB.                   
030820                                                                          
030900     PERFORM IMS-GET-MSG                                                  
031000     IF SEGMENT-FINNS                                                     
031100         PERFORM A-INIT                                                   
031200*                                                                         
031300         IF WS-SAMMA-BILD                                                 
031400         OR WS-GODKAND-BILD                                               
031500             PERFORM B-GENERELL-KONTROLL                                  
031600             IF WS-INDATA-RATT                                            
031700                 PERFORM IMS-GU-KOLLIREG                                  
031800                 IF SEGMENT-FINNS                                         
032000                     PERFORM C-BESTAM-PRINTER                             
032100                     PERFORM D-INIT-PLIST                                 
032200                     PERFORM E-REDIGERA-HUVUDRAD                          
032300                     PERFORM IMS-GU-KOLLIREG                              
032400                     PERFORM IMS-GNP-KOLLI                                
032500                     PERFORM UNTIL (                                      
032600                        SEGMENT-SAKNAS OR                                 
032700                        ACK-ANTAL-SIDOR = 24)                             
032800                         PERFORM S01-SKRIV-RUBRIK-1-2                     
032900                         PERFORM S02-SKRIV-HUVUDRAD                       
033000                         PERFORM S03-SKRIV-RUBRIKRAD-3                    
033100                         PERFORM UNTIL (                                  
033200                            SEGMENT-SAKNAS OR                             
033300                            ACK-ANTAL-RADER = 44)                         
033400                             PERFORM F-REDIGERA-KOLLI-RAD                 
033500                             PERFORM S04-SKRIV-DETALJRAD                  
033600                             PERFORM IMS-GNP-KOLLI                        
033700                         END-PERFORM                                      
033800                     END-PERFORM                                          
033910                     PERFORM H-AVSLUT                                     
034000                 ELSE                                                     
034100                     MOVE FEL            TO  WS-INDATA-TEST               
034200                     MOVE FEL-749 (INDX) TO  MOD-TEMFSFEL                 
034300                 END-IF                                                   
034400             ELSE                                                         
034500                 MOVE FEL            TO  WS-INDATA-TEST                   
034600                 MOVE FEL-749 (INDX) TO  MOD-TEMFSFEL                     
034700             END-IF                                                       
034800             MOVE MAX-MOD-LAENGD TO  MSG-KVLL                             
034900         ELSE                                                             
035000             MOVE '4342'         TO  MFS-IDTRANS                          
035100             MOVE +8             TO  MSG-KVLL                             
035200             MOVE FEL            TO  WS-INDATA-TEST                       
035300             MOVE 'W4O34201'     TO  MFS-IDMOD                            
035400         END-IF                                                           
035500         IF MFS-IDTRANS = '4341' OR '4342'                                
035600             PERFORM IMS-INSERT-MSG                                       
035700         END-IF                                                           
035800     END-IF                                                               
035900     MOVE ZERO TO RETURN-CODE                                             
036000     GOBACK                                                               
036100     .                                                                    
036200     EJECT                                                                
036300 A-INIT             SECTION.                                              
036400     SKIP3                                                                
036500     IF MSG-DUBBLA-TRANSKODER                                             
036600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO   MID-W4I34201               
036700       MOVE MSG-IDTRANS-2                 TO   MFS-IDTRANS                
036800                                               WS-IDTRANS                 
036900       MOVE MSG-KDMFSFOR-2                TO   MFS-KDMFSFOR               
037000       MOVE MSG-KDTRTYP                   TO   MFS-KDTRTYP                
037100     ELSE                                                                 
037200       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO   MID-W4I34201               
037300       MOVE MSG-IDTRANS-1                 TO   MFS-IDTRANS                
037400                                               WS-IDTRANS                 
037500       MOVE MSG-KDMFSFOR-1                TO   MFS-KDMFSFOR               
037600       MOVE ' '                           TO   MFS-KDTRTYP                
037700     END-IF                                                               
037800*                                                                         
037900     MOVE LOW-VALUE                       TO   MSG-AREA                   
038000     MOVE 'W4O342N1'                      TO   MFS-IDMOD                  
038100     MOVE '4342'                          TO   MOD-IDTRANS                
038120     MOVE ZERO                            TO  W-Q301-MIN-IDORDER          
038140                                              W-Q301-MIN-IDPRODNR         
038150                                              W-Q301-MIN-IDPLKLST         
038160                                              W-Q301-MAX-IDORDER          
038170                                              W-Q301-MAX-IDPRODNR         
038180                                              W-Q301-MAX-IDPLKLST         
038200*                                                                         
038300     IF ENGLISH-TEXT                                                      
038400         MOVE +2                          TO   INDX                       
038500     ELSE                                                                 
038600         MOVE +1                          TO   INDX                       
038700     END-IF                                                               
038800     MOVE RAETT                           TO WS-INDATA-TEST               
038900*                                                                         
039000     IF MID-IDPRODNR-IN = ALL '+'                                         
039100         MOVE MID-IDPRODNR-UT             TO   WS-IDPRODNR                
039200         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
039300     ELSE                                                                 
039400         MOVE MID-IDPRODNR-IN             TO   WS-IDPRODNR                
039500     END-IF                                                               
039600*                                                                         
039700*    --- INPUT WAREHOUSE IDENTIFIER                                       
039800*                                                                         
039900     IF MID-IDDC-IN = ALL '+'                                             
040000       IF MID-IDDC-UT = SPACE                                             
040010         MOVE FEL                       TO   WS-INDATA-TEST               
040200       ELSE                                                               
040300         MOVE MID-IDDC-UT               TO   MOD-IDDC-UT                  
040400       END-IF                                                             
040500     ELSE                                                                 
040600       MOVE MID-IDDC-IN                 TO   MOD-IDDC-UT                  
040700     END-IF                                                               
040800                                                                          
040900     MOVE WS-IDPRODNR                   TO   MOD-IDPRODNR-UT              
041000     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
041100     MOVE MFS-RENSA-FAELT               TO   MOD-IDPRODNR-IN              
041300     MOVE MFS-RENSA-FAELT               TO   MOD-IDDC-IN                  
041301                                                                          
041302     IF WS-INDATA-RATT                                                    
041310        MOVE MOD-IDDC-UT                   TO   W-IDDC-B6                 
041311        IF W-IDDC-B6 IS > SPACE                                           
041320           PERFORM IMS-GU-WDB601                                          
041330        END-IF                                                            
041340     END-IF                                                               
041400     .                                                                    
041500     EJECT                                                                
041600 B-GENERELL-KONTROLL  SECTION.                                            
041700     SKIP3                                                                
041800     IF WS-IDPRODNR NOT NUMERIC                                           
041900         MOVE FEL                       TO   WS-INDATA-TEST               
042000         MOVE FEL-749 (INDX)            TO   MOD-TEMFSFEL                 
042100     ELSE                                                                 
042200         MOVE WS-IDPRODNR               TO   W-601-IDPRODNR               
042300     END-IF                                                               
042400                                                                          
042500     IF DCS-KDDC = SPACE                                                  
042510     OR DCS-DDC                                                           
042900       MOVE FEL                         TO   WS-INDATA-TEST               
043000     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 C-BESTAM-PRINTER     SECTION.                                            
043400     SKIP3                                                                
043500     IF DCS-SDC AND DCS-IDLANDX2 NOT = 'SE'                               
043600         MOVE PACK-C2                   TO  WS-LISTA                      
043700     ELSE                                                                 
043800         MOVE VORD-IDDISTR              TO  DIST48-IDDISTR                
043900         IF DIST48-NORDEN                                                 
044000             MOVE PACK-NORDEN-C1        TO  WS-LISTA                      
044100         ELSE                                                             
044200             MOVE PACK-EXP-C1           TO  WS-LISTA                      
044300         END-IF                                                           
044400     END-IF                                                               
044500     .                                                                    
044600     EJECT                                                                
044700 D-INIT-PLIST             SECTION.                                        
044800     SKIP3                                                                
044900     MOVE 'W40342-001'                 TO  PRT-IDLIST                     
045000     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
045100                         PRT-OPEN                                         
045200                         WS-LISTA                                         
045300                         ALT-PCB                                          
045400                         WS-DUMMY                                         
045500                         WS-DUMMY                                         
045600     .                                                                    
045700     EJECT                                                                
045800 E-REDIGERA-HUVUDRAD      SECTION.                                        
045900     SKIP3                                                                
046000     IF DCS-SDC AND DCS-IDLANDX2 = 'SE'                                   
046010       MOVE VORD-IDDISTR             TO LDC-RAD1-IDDISTR                  
046011       MOVE VORD-IDKUNDNR            TO LDC-RAD1-IDKUNDNR                 
046012       MOVE VORD-IDDC                TO LDC-RAD1-IDDC                     
046013       MOVE VORD-KDFRAKT             TO LDC-RAD1-KDFRAKT                  
046014       MOVE VORD-KDORDKL             TO LDC-RAD1-KDORDKL                  
046015       MOVE VORD-IDPRODNR            TO LDC-RAD1-IDPRODNR                 
046016       MOVE VORD-IDPRODNR            TO W-IDPRODNR-WDE4F-MAX              
046017       MOVE VORD-IDPRODNR            TO W-IDPRODNR-WDE4F-MIN              
046018       MOVE VORD-VLORDBTO            TO LDC-RAD1-VLORDBTO                 
046019       MOVE VORD-VKORDBTO            TO LDC-RAD1-VKORDBTO                 
046020       MOVE VORD-KVKOLLI             TO LDC-RAD1-KVKOLLI                  
046021*      MOVE VORD-TIPACKN-SK          TO LDC-RAD1-TIPACKN                  
046022     ELSE                                                                 
046030       MOVE VORD-IDDISTR             TO RAD1-IDDISTR                      
046100       MOVE VORD-IDKUNDNR            TO RAD1-IDKUNDNR                     
046200       MOVE VORD-IDDC                TO RAD1-IDDC                         
046300       MOVE VORD-KDFRAKT             TO RAD1-KDFRAKT                      
046400       MOVE VORD-KDORDKL             TO RAD1-KDORDKL                      
046500       MOVE VORD-IDPRODNR            TO RAD1-IDPRODNR                     
046510       MOVE VORD-IDPRODNR            TO W-IDPRODNR-WDE4F-MAX              
046520       MOVE VORD-IDPRODNR            TO W-IDPRODNR-WDE4F-MIN              
046600       MOVE VORD-VLORDBTO            TO RAD1-VLORDBTO                     
046700       MOVE VORD-VKORDBTO            TO RAD1-VKORDBTO                     
046800       MOVE VORD-KVKOLLI             TO RAD1-KVKOLLI                      
046900       MOVE VORD-TIPACKN-SK          TO RAD1-TIPACKN                      
046910     END-IF                                                               
047000     SKIP2                                                                
047100     PERFORM IMS-GN-WDE4F                                                 
047200     IF SEGMENT-FINNS                                                     
047300         MOVE SEQF-IDKUNDRF          TO WS-IDKUNDRF                       
047400         MOVE WS-IDORDNR             TO RAD1-IDORDNR                      
047410         MOVE WS-IDORDNR             TO LDC-RAD1-IDORDNR                  
047500     ELSE                                                                 
047600         MOVE ZERO                   TO RAD1-IDORDNR                      
047610         MOVE ZERO                   TO LDC-RAD1-IDORDNR                  
047700     END-IF                                                               
047800     .                                                                    
047900     EJECT                                                                
048000 F-REDIGERA-KOLLI-RAD     SECTION.                                        
048100     SKIP3                                                                
048110     IF DCS-SDC AND DCS-IDLANDX2 = 'SE'                                   
048200       MOVE KOLLI-IDKOLLI            TO LDC-RAD2-IDKOLLI                  
048300       MOVE KOLLI-KDKOLLI            TO LDC-RAD2-KDKOLLI                  
048400       MOVE KOLLI-KDEMBTYP           TO LDC-RAD2-KDEMBTYP                 
048500       MOVE KOLLI-DIKOLLIL           TO LDC-RAD2-DIKOLLIL                 
048600       MOVE KOLLI-DIKOLLIB           TO LDC-RAD2-DIKOLLIB                 
048700       MOVE KOLLI-DIKOLLIH           TO LDC-RAD2-DIKOLLIH                 
048800       MOVE KOLLI-VLORDBTO-KOLLI     TO LDC-RAD2-VLORDBTO                 
048900       MOVE KOLLI-VKORDBTO-KOLLI     TO LDC-RAD2-VKORDBTO                 
049000       MOVE KOLLI-TIPACKN            TO LDC-RAD2-TIPACKN                  
049401     ELSE                                                                 
049410       MOVE KOLLI-IDKOLLI            TO RAD2-IDKOLLI                      
049420       MOVE KOLLI-KDKOLLI            TO RAD2-KDKOLLI                      
049430       MOVE KOLLI-KDEMBTYP           TO RAD2-KDEMBTYP                     
049440       MOVE KOLLI-DIKOLLIL           TO RAD2-DIKOLLIL                     
049450       MOVE KOLLI-DIKOLLIB           TO RAD2-DIKOLLIB                     
049460       MOVE KOLLI-DIKOLLIH           TO RAD2-DIKOLLIH                     
049470       MOVE KOLLI-VLORDBTO-KOLLI     TO RAD2-VLORDBTO                     
049480       MOVE KOLLI-VKORDBTO-KOLLI     TO RAD2-VKORDBTO                     
049490       MOVE KOLLI-TIPACKN            TO RAD2-TIPACKN                      
049491       MOVE KOLLI-ADFLGEO            TO RAD2-ADFLGEO                      
049492       MOVE KOLLI-ADFLOMR            TO RAD2-ADFLOMR                      
049493       MOVE KOLLI-ADRUTNIV           TO RAD2-ADRUTNIV                     
049494       MOVE KOLLI-ADVMODUL           TO RAD2-ADVMODUL                     
049495     END-IF                                                               
049500     IF  KOLLI-KDFARLIG-KOLLI = +4                                        
049510     OR  KOLLI-KDFARLIG-KOLLI = +7                                        
049600         MOVE 'JA ' TO RAD2-FARLIG                                        
049610         MOVE 'JA ' TO LDC-RAD2-FARLIG                                    
049700     ELSE                                                                 
049800         MOVE SPACE TO RAD2-FARLIG                                        
049810         MOVE SPACE TO LDC-RAD2-FARLIG                                    
049900     END-IF                                                               
050000                                                                          
050100     .                                                                    
050200     EJECT                                                                
050300 H-AVSLUT             SECTION.                                            
050310                                                                          
050500     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
050600                         PRT-CLOSE                                        
050700                         WS-LISTA                                         
050800                         ALT-PCB                                          
050900                         WS-DUMMY                                         
051000                         WS-DUMMY                                         
051100     .                                                                    
051200     EJECT                                                                
051300 S01-SKRIV-RUBRIK-1-2 SECTION.                                            
051400     SKIP3                                                                
051500     ADD +1                  TO   ACK-ANTAL-SIDOR                         
051510     EVALUATE TRUE                                                        
051520     WHEN DCS-CDC                                                         
051700       MOVE ACK-ANTAL-SIDOR    TO   RUB-1-SIDNR-SVE                       
051800       ACCEPT RUB-1-DATUM-SVE  FROM DATE                                  
051900       MOVE +7                 TO   ACK-ANTAL-RADER                       
052000                                                                          
052100       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
052200                           PRT-WRITE                                      
052300                           WS-LISTA                                       
052400                           ALT-PCB                                        
052500                           PRT-NYSIDA-RAD4                                
052600                           RUBRIKRAD-1-SVE                                
052700                                                                          
052800       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
052900                           PRT-WRITE                                      
053000                           WS-LISTA                                       
053100                           ALT-PCB                                        
053200                           PRT-AFTER-3                                    
053300                           RUBRIKRAD-2-SVE                                
053310     WHEN DCS-SDC AND DCS-IDLANDX2 = 'SE'                                 
053311       MOVE ACK-ANTAL-SIDOR    TO   RUB-1-SIDNR-SVE                       
053312       ACCEPT RUB-1-DATUM-SVE  FROM DATE                                  
053313       MOVE +7                 TO   ACK-ANTAL-RADER                       
053314                                                                          
053315       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
053316                           PRT-WRITE                                      
053317                           WS-LISTA                                       
053318                           ALT-PCB                                        
053319                           PRT-NYSIDA-RAD4                                
053320                           RUBRIKRAD-1-SVE                                
053321                                                                          
053322       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
053323                           PRT-WRITE                                      
053324                           WS-LISTA                                       
053325                           ALT-PCB                                        
053326                           PRT-AFTER-3                                    
053327                           RUBRIKRAD-2-LDC                                
053330     WHEN OTHER                                                           
053500       MOVE ACK-ANTAL-SIDOR    TO   RUB-1-SIDNR-ENG                       
053600       ACCEPT RUB-1-DATUM-ENG  FROM DATE                                  
053700       MOVE +7                 TO   ACK-ANTAL-RADER                       
053800                                                                          
053900       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
054000                           PRT-WRITE                                      
054100                           WS-LISTA                                       
054200                           ALT-PCB                                        
054300                           PRT-NYSIDA-RAD4                                
054400                           RUBRIKRAD-1-ENG                                
054500                                                                          
054600       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
054700                           PRT-WRITE                                      
054800                           WS-LISTA                                       
054900                           ALT-PCB                                        
055000                           PRT-AFTER-3                                    
055100                           RUBRIKRAD-2-ENG                                
055200     END-EVALUATE                                                         
055300     .                                                                    
055400     EJECT                                                                
055500 S02-SKRIV-HUVUDRAD   SECTION.                                            
055600     SKIP3                                                                
055700     ADD +1                  TO  ACK-ANTAL-RADER                          
055800     SKIP2                                                                
055900     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
056000                         PRT-WRITE                                        
056100                         WS-LISTA                                         
056200                         ALT-PCB                                          
056300                         PRT-AFTER-1                                      
056400                         HUVUDRAD                                         
056500     .                                                                    
056600     EJECT                                                                
056700 S03-SKRIV-RUBRIKRAD-3 SECTION.                                           
056800     SKIP3                                                                
056900     ADD +2                  TO  ACK-ANTAL-RADER                          
057000     SKIP2                                                                
057010     EVALUATE TRUE                                                        
057100     WHEN DCS-CDC                                                         
057200       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
057300                           PRT-WRITE                                      
057400                           WS-LISTA                                       
057500                           ALT-PCB                                        
057600                           PRT-AFTER-2                                    
057700                           RUBRIKRAD-3-SVE                                
057710     WHEN DCS-SDC AND DCS-IDLANDX2 = 'SE'                                 
057720       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
057730                           PRT-WRITE                                      
057740                           WS-LISTA                                       
057750                           ALT-PCB                                        
057760                           PRT-AFTER-2                                    
057770                           RUBRIKRAD-3-LDC                                
057800     WHEN OTHER                                                           
057900       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
058000                           PRT-WRITE                                      
058100                           WS-LISTA                                       
058200                           ALT-PCB                                        
058300                           PRT-AFTER-2                                    
058400                           RUBRIKRAD-3-ENG                                
058500     END-EVALUATE                                                         
058600     .                                                                    
058700     EJECT                                                                
058800 S04-SKRIV-DETALJRAD   SECTION.                                           
058900     SKIP3                                                                
059000     ADD +1                  TO  ACK-ANTAL-RADER                          
059100     SKIP2                                                                
059110     EVALUATE TRUE                                                        
059130     WHEN DCS-SDC AND DCS-IDLANDX2 = 'SE'                                 
059200       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
059300                           PRT-WRITE                                      
059400                           WS-LISTA                                       
059500                           ALT-PCB                                        
059600                           PRT-AFTER-1                                    
059700                           DETALJRAD-LDC                                  
059701     WHEN OTHER                                                           
059702       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
059703                           PRT-WRITE                                      
059704                           WS-LISTA                                       
059705                           ALT-PCB                                        
059706                           PRT-AFTER-1                                    
059707                           DETALJRAD                                      
059710     END-EVALUATE                                                         
059800     .                                                                    
059900     EJECT                                                                
060000* IMS SEKTIONER                                                           
060100     SKIP3                                                                
060200 IMS-GET-MSG SECTION.                                                     
060300     MOVE '  QC' TO GODK-STATUSKODER                                      
060400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
060500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060600     PERFORM IMS-STATUSKONTROLL                                           
060700     .                                                                    
060800     SKIP3                                                                
060900 IMS-INSERT-MSG SECTION.                                                  
061000     IF NOT ENGLISH-TEXT                                                  
061100       MOVE '0' TO MFS-KDHUVOMR                                           
061200     END-IF                                                               
061300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
061400     MOVE SPACE TO GODK-STATUSKODER                                       
061500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
061600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061700     PERFORM IMS-STATUSKONTROLL                                           
061800     .                                                                    
063000     EJECT                                                                
063100 IMS-GU-KOLLIREG SECTION.                                                 
063200     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
063300            DELIMITED BY SIZE INTO SSA1                                   
063400     MOVE '  GE' TO GODK-STATUSKODER                                      
063500     CALL CBLTDLI USING GU   WDE6-PCB DLI-IO-AREA SSA1                    
063600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     .                                                                    
063900     SKIP3                                                                
064000 IMS-GNP-KOLLI           SECTION.                                         
064100     MOVE 'WDE611   ' TO SSA1                                             
064200     MOVE '  GE' TO GODK-STATUSKODER                                      
064300     CALL CBLTDLI USING GNP  WDE6-PCB DLI-IO-AREA SSA1                    
064400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
064500     PERFORM IMS-STATUSKONTROLL                                           
064600     .                                                                    
064700     SKIP3                                                                
065605 IMS-GN-WDE4F SECTION.                                                    
065606                                                                          
065607     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
065608                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
065609          DELIMITED BY SIZE INTO SSA1                                     
065610     MOVE '  GE' TO GODK-STATUSKODER                                      
065611     CALL CBLTDLI USING GN WDE4F-PCB WDE4F-AREA SSA1                      
065612     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
065613     PERFORM IMS-STATUSKONTROLL                                           
065614     .                                                                    
065615                                                                          
065616 IMS-GU-WDB601    SECTION.                                                
065617     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
065618          DELIMITED BY SIZE INTO SSA1                                     
065619     MOVE '  GE' TO GODK-STATUSKODER                                      
065620     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
065621     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
065622     PERFORM IMS-STATUSKONTROLL                                           
065623     IF SEGMENT-SAKNAS                                                    
065624        MOVE SPACE TO DCS-KDDC                                            
065625     END-IF                                                               
065626     .                                                                    
065630     SKIP2                                                                
065640 IMS-STATUSKONTROLL SECTION.                                              
065700     SET STATUS-IX TO 1                                                   
065800     SEARCH GODK-STATUS                                                   
065900       AT END CALL FELLOG                                                 
066000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
066100     END-SEARCH                                                           
066200     .                                                                    
