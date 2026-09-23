000100                                                                          
000200                                                                          
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W5602300.                                                
000500 AUTHOR.         BARSHARANI BISHOYE.                                      
000600 DATE-WRITTEN.   21/08/24.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        LAB REPORT FOR US01 AND CA03                                     
001200*                                                                         
001300*                                                                         
001400*    ABENDCODES:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- US01 DATA                                                  
002700     SELECT W56022                     ASSIGN TO W56023D1.                
002800     SKIP2                                                                
002900*          --- CA03 DATA                                                  
003000     SELECT W56023                     ASSIGN TO W56023D2.                
003100     SKIP2                                                                
003200*          --- REPORT WITH US01 DATA                                      
003300     SELECT W56022A                    ASSIGN TO W56023D3.                
003400     SKIP2                                                                
003500*          --- REPORT WITH CA03 DATA                                      
003600     SELECT W56023A                    ASSIGN TO W56023D4.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W56022                                                               
004300     RECORDING       V                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W510AX2      -L.                                               
004700                                                                          
004800*01  -COPY W510A01      -L.                                               
004900                                                                          
005000*01  -COPY W510A02      -L.                                               
005100                                                                          
005200*01  -COPY W510A03      -L.                                               
005300                                                                          
005400*01  -COPY W510A04      -L.                                               
005500                                                                          
005600*01  -COPY W510A05      -L.                                               
005700                                                                          
005800*01  -COPY W510A06      -L.                                               
005900                                                                          
006000*01  -COPY W510A07      -L.                                               
006100                                                                          
006200*01  -COPY W510A08      -L.                                               
006300                                                                          
006400*01  -COPY W510A09      -L.                                               
006500                                                                          
006600*01  -COPY W510A10      -L.                                               
006700                                                                          
006800*01  -COPY W510A11      -L.                                               
006900                                                                          
007000*01  -COPY W510A12      -L.                                               
007100                                                                          
007200*01  -COPY W510A13      -L.                                               
007300                                                                          
007400*01  -COPY W510A14      -L.                                               
007500                                                                          
007600*01  -COPY W510A15      -L.                                               
007700                                                                          
007800*01  -COPY W510A16      -L.                                               
007900                                                                          
008000*01  -COPY W510A17      -L.                                               
008100                                                                          
008200*01  -COPY W510A18      -L.                                               
008300                                                                          
008400*01  -COPY W510A19      -L.                                               
008500                                                                          
008600*01  -COPY W510A20      -L.                                               
008700                                                                          
008800*01  -COPY W510L09      -L.                                               
008900     SKIP3                                                                
009000 FD  W56023                                                               
009100     RECORDING       V                                                    
009200     BLOCK CONTAINS  0.                                                   
009300                                                                          
009400*01  -COPY W510AX2      -L.                                               
009500                                                                          
009600*01  -COPY W510A01      -L.                                               
009700                                                                          
009800*01  -COPY W510A02      -L.                                               
009900                                                                          
010000*01  -COPY W510A03      -L.                                               
010100                                                                          
010200*01  -COPY W510A04      -L.                                               
010300                                                                          
010400*01  -COPY W510A05      -L.                                               
010500                                                                          
010600*01  -COPY W510A06      -L.                                               
010700                                                                          
010800*01  -COPY W510A07      -L.                                               
010900                                                                          
011000*01  -COPY W510A08      -L.                                               
011100                                                                          
011200*01  -COPY W510A09      -L.                                               
011300                                                                          
011400*01  -COPY W510A10      -L.                                               
011500                                                                          
011600*01  -COPY W510A11      -L.                                               
011700                                                                          
011800*01  -COPY W510A12      -L.                                               
011900                                                                          
012000*01  -COPY W510A13      -L.                                               
012100                                                                          
012200*01  -COPY W510A14      -L.                                               
012300                                                                          
012400*01  -COPY W510A15      -L.                                               
012500                                                                          
012600*01  -COPY W510A16      -L.                                               
012700                                                                          
012800*01  -COPY W510A17      -L.                                               
012900                                                                          
013000*01  -COPY W510A18      -L.                                               
013100                                                                          
013200*01  -COPY W510A19      -L.                                               
013300                                                                          
013400*01  -COPY W510A20      -L.                                               
013500                                                                          
013600*01  -COPY W510L09      -L.                                               
013700     SKIP3                                                                
013800 FD  W56022A                                                              
013900     RECORDING       V                                                    
014000     BLOCK CONTAINS  0.                                                   
014100 01  LISTPOST1                    PIC X(1137).                            
014200     SKIP3                                                                
014300 FD  W56023A                                                              
014400     RECORDING       V                                                    
014500     BLOCK CONTAINS  0.                                                   
014600 01  LISTPOST2                    PIC X(1137).                            
014700     EJECT                                                                
014800 WORKING-STORAGE SECTION.                                                 
014900                                                                          
015000 77  IDPGM                       PIC X(8)    VALUE 'W5602300'.            
015100 77  YES                         PIC X       VALUE 'J'.                   
015200 77  NOO                         PIC X       VALUE 'N'.                   
015300                                                                          
015400 77  W56022-EOF-SW               PIC X       VALUE 'N'.                   
015500                                                                          
015600 77  W56023-EOF-SW               PIC X       VALUE 'N'.                   
015700     EJECT                                                                
015800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
015900 01  FILLER REDEFINES TODAYS-DATE.                                        
016000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
016100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
016200     03  TODAYS-DATE-DAY         PIC 9(2).                                
016300     EJECT                                                                
016400 01  GENERAL-SUBPROGRAMS.                                                 
016500*                                                                         
016600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
016700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
016800     SKIP2                                                                
016900*    --- PARAMETERS TO ABEND                                              
017000                                                                          
017100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
017200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
017300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
017400     SKIP2                                                                
017500 01  ERROR-TEXT.                                                          
017600     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
017700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL POSTSUM                                          
018000*                                                                         
018100*01  -COPY W0005   -PRE  POSTSUM-                                         
018200     EJECT                                                                
018600 01  IN1-AREA.                                                            
018700     03  IN1-AREA-0.                                                      
018800       05  IN1-IDPTYP           PIC X(3).                                 
018900       05  FILLER               PIC X(200).                               
019100*   03  FILLER -COPY W510AX2  -PRE 1AX2-   -RED  IN1-AREA-0               
019200*   03  FILLER -COPY W510A01  -PRE 1A01-   -RED  IN1-AREA-0               
019300*   03  FILLER -COPY W510A02  -PRE 1A02-   -RED  IN1-AREA-0               
019400*   03  FILLER -COPY W510A03  -PRE 1A03-   -RED  IN1-AREA-0               
019500*   03  FILLER -COPY W510A04  -PRE 1A04-   -RED  IN1-AREA-0               
019600*   03  FILLER -COPY W510A05  -PRE 1A05-   -RED  IN1-AREA-0               
019700*   03  FILLER -COPY W510A06  -PRE 1A06-   -RED  IN1-AREA-0               
019800*   03  FILLER -COPY W510A07  -PRE 1A07-   -RED  IN1-AREA-0               
019900*   03  FILLER -COPY W510A08  -PRE 1A08-   -RED  IN1-AREA-0               
020000*   03  FILLER -COPY W510A09  -PRE 1A09-   -RED  IN1-AREA-0               
020100*   03  FILLER -COPY W510A10  -PRE 1A10-   -RED  IN1-AREA-0               
020200*   03  FILLER -COPY W510A11  -PRE 1A11-   -RED  IN1-AREA-0               
020300*   03  FILLER -COPY W510A12  -PRE 1A12-   -RED  IN1-AREA-0               
020400*   03  FILLER -COPY W510A13  -PRE 1A13-   -RED  IN1-AREA-0               
020500*   03  FILLER -COPY W510A14  -PRE 1A14-   -RED  IN1-AREA-0               
020600*   03  FILLER -COPY W510A15  -PRE 1A15-   -RED  IN1-AREA-0               
020700*   03  FILLER -COPY W510A16  -PRE 1A16-   -RED  IN1-AREA-0               
020800*   03  FILLER -COPY W510A17  -PRE 1A17-   -RED  IN1-AREA-0               
020900*   03  FILLER -COPY W510A18  -PRE 1A18-   -RED  IN1-AREA-0               
021000*   03  FILLER -COPY W510A19  -PRE 1A19-   -RED  IN1-AREA-0               
021100*   03  FILLER -COPY W510A20  -PRE 1A20-   -RED  IN1-AREA-0               
021200*   03  FILLER -COPY W510L09  -PRE 1L09-   -RED  IN1-AREA-0               
021300     EJECT                                                                
021700 01  IN2-AREA.                                                            
021800     03  IN2-AREA-0.                                                      
021900       05  IN2-IDPTYP           PIC X(3).                                 
022000       05  FILLER               PIC X(200).                               
022200*   03  FILLER -COPY W510AX2  -PRE 2AX2-   -RED  IN2-AREA-0               
022300*   03  FILLER -COPY W510A01  -PRE 2A01-   -RED  IN2-AREA-0               
022400*   03  FILLER -COPY W510A02  -PRE 2A02-   -RED  IN2-AREA-0               
022500*   03  FILLER -COPY W510A03  -PRE 2A03-   -RED  IN2-AREA-0               
022600*   03  FILLER -COPY W510A04  -PRE 2A04-   -RED  IN2-AREA-0               
022700*   03  FILLER -COPY W510A05  -PRE 2A05-   -RED  IN2-AREA-0               
022800*   03  FILLER -COPY W510A06  -PRE 2A06-   -RED  IN2-AREA-0               
022900*   03  FILLER -COPY W510A07  -PRE 2A07-   -RED  IN2-AREA-0               
023000*   03  FILLER -COPY W510A08  -PRE 2A08-   -RED  IN2-AREA-0               
023100*   03  FILLER -COPY W510A09  -PRE 2A09-   -RED  IN2-AREA-0               
023200*   03  FILLER -COPY W510A10  -PRE 2A10-   -RED  IN2-AREA-0               
023300*   03  FILLER -COPY W510A11  -PRE 2A11-   -RED  IN2-AREA-0               
023400*   03  FILLER -COPY W510A12  -PRE 2A12-   -RED  IN2-AREA-0               
023500*   03  FILLER -COPY W510A13  -PRE 2A13-   -RED  IN2-AREA-0               
023600*   03  FILLER -COPY W510A14  -PRE 2A14-   -RED  IN2-AREA-0               
023700*   03  FILLER -COPY W510A15  -PRE 2A15-   -RED  IN2-AREA-0               
023800*   03  FILLER -COPY W510A16  -PRE 2A16-   -RED  IN2-AREA-0               
023900*   03  FILLER -COPY W510A17  -PRE 2A17-   -RED  IN2-AREA-0               
024000*   03  FILLER -COPY W510A18  -PRE 2A18-   -RED  IN2-AREA-0               
024100*   03  FILLER -COPY W510A19  -PRE 2A19-   -RED  IN2-AREA-0               
024200*   03  FILLER -COPY W510A20  -PRE 2A20-   -RED  IN2-AREA-0               
024300*   03  FILLER -COPY W510L09  -PRE 2L09-   -RED  IN2-AREA-0               
024400     EJECT                                                                
024700     SKIP2                                                                
024800 01  UT1-HEADER.                                                          
024900     03 UT1H-IDPTYP          PIC X(11)    VALUE 'RECORD TYPE'.            
025000     03 FILLER              PIC X(1)     VALUE ';'.                       
025100     03 UT1H-KDEKOHT         PIC X(14)    VALUE 'ECONOMIC EVENT'.         
025200     03 FILLER              PIC X(1)     VALUE ';'.                       
025300     03 UT1H-IDFTG           PIC X(13)    VALUE 'COMPANY ID NO'.          
025400     03 FILLER              PIC X(1)     VALUE ';'.                       
025500     03 UT1H-IDDC-SEND       PIC X(7)     VALUE 'SEND DC'.                
025600     03 FILLER              PIC X(1)     VALUE ';'.                       
025700     03 UT1H-IDDC-REC        PIC X(6)     VALUE 'REC DC'.                 
025800     03 FILLER              PIC X(1)     VALUE ';'.                       
025900     03 UT1H-IDDISTR         PIC X(8)     VALUE 'DISTRICT'.               
026000     03 FILLER              PIC X(1)     VALUE ';'.                       
026100     03 UT1H-IDKUNDNR        PIC X(8)     VALUE 'CUSTOMER'.               
026200     03 FILLER              PIC X(1)     VALUE ';'.                       
026300     03 UT1H-IDFAKT          PIC X(6)     VALUE 'INV NO'.                 
026400     03 FILLER              PIC X(1)     VALUE ';'.                       
026500     03 UT1H-KDFAKTYP        PIC X(8)     VALUE 'INV TYPE'.               
026600     03 FILLER              PIC X(1)     VALUE ';'.                       
026700     03 UT1H-DAFAKT          PIC X(8)     VALUE 'INV DATE'.               
026800     03 FILLER              PIC X(1)     VALUE ';'.                       
026900     03 UT1H-IDORDNR7        PIC X(6)     VALUE 'ORD NO'.                 
027000     03 FILLER              PIC X(1)     VALUE ';'.                       
027100     03 UT1H-IDARTNR         PIC X(7)     VALUE 'PART NO'.                
027200     03 FILLER              PIC X(1)     VALUE ';'.                       
027300     03 UT1H-KDPRODSL        PIC X(8)     VALUE 'PROD GRP'.               
027400     03 FILLER              PIC X(1)     VALUE ';'.                       
027500     03 UT1H-KDPSLLOC        PIC X(12)    VALUE 'LOC PROD GRP'.           
027600     03 FILLER              PIC X(1)     VALUE ';'.                       
027700     03 UT1H-KVLEVART        PIC X(3)     VALUE 'QTY'.                    
027800     03 FILLER              PIC X(1)     VALUE ';'.                       
027900     03 UT1H-PRARTNTO        PIC X(9)     VALUE 'NET PRICE'.              
028000     03 FILLER              PIC X(1)     VALUE ';'.                       
028100     03 UT1H-FLOVRLEV        PIC X(13)    VALUE 'OVER DELIVERY'.          
028200     03 FILLER              PIC X(1)     VALUE ';'.                       
028300     03 UT1H-KDFRAKT         PIC X(12)    VALUE 'FREIGHT CODE'.           
028400     03 FILLER              PIC X(1)     VALUE ';'.                       
028500     03 UT1H-SUFAKTRE        PIC X(17)    VALUE                           
028510                                         'SALES SPARE PARTS'.             
028600     03 FILLER              PIC X(1)     VALUE ';'.                       
028700     03 UT1H-PREMBHNT        PIC X(20)    VALUE                           
028710                                         'PACKING & HANDL COST'.          
028800     03 FILLER              PIC X(1)     VALUE ';'.                       
028900     03 UT1H-PRFRAKT         PIC X(12)    VALUE 'FREIGHT COST'.           
029000     03 FILLER              PIC X(1)     VALUE ';'.                       
029100     03 UT1H-PRFOERS         PIC X(9)     VALUE 'INSURANCE'.              
029200     03 FILLER              PIC X(1)     VALUE ';'.                       
029300     03 UT1H-PRMOMS          PIC X(3)     VALUE 'VAT'.                    
029400     03 FILLER              PIC X(1)     VALUE ';'.                       
029500     03 UT1H-PRLEGKST        PIC X(10)    VALUE 'LEGAL COST'.             
029600     03 FILLER              PIC X(1)     VALUE ';'.                       
029700     03 UT1H-SUFKTTILL      PIC X(15)    VALUE 'ADDITIONAL COST'.         
029800     03 FILLER              PIC X(1)     VALUE ';'.                       
029900     03 UT1H-PRAVDRAG        PIC X(13)    VALUE 'DEDUCTION INV'.          
030000     03 FILLER              PIC X(1)     VALUE ';'.                       
030100     03 UT1H-SUFKTBEL        PIC X(17)    VALUE                           
030110                                         'TOT INVOICED AMNT'.             
030200     03 FILLER              PIC X(1)     VALUE ';'.                       
030300     03 UT1H-SUFKTUTL        PIC X(7)     VALUE 'INV SUM'.                
030400     03 FILLER              PIC X(1)     VALUE ';'.                       
030500     03 UT1H-PRKURS          PIC X(18)    VALUE                           
030510                                         'CURR EXCHANGE RATE'.            
030600     03 FILLER              PIC X(1)     VALUE ';'.                       
030700     03 UT1H-IDRAPPNR        PIC X(18)    VALUE                           
030710                                         'DISCREPANCY RPT NO'.            
030800     03 FILLER              PIC X(1)     VALUE ';'.                       
030900     03 UT1H-IDKOLLI         PIC X(7)     VALUE 'CASE NO'.                
031000     03 FILLER              PIC X(1)     VALUE ';'.                       
031100     03 UT1H-KDANMORS        PIC X(14)    VALUE 'DISCR RPT CODE'.         
031200     03 FILLER              PIC X(1)     VALUE ';'.                       
031300     03 UT1H-KDVALISO        PIC X(9)     VALUE 'CURR CODE'.              
031400     03 FILLER              PIC X(1)     VALUE ';'.                       
031500     03 UT1H-DAINLINL        PIC X(14)    VALUE 'REPORTING DATE'.         
031600     03 FILLER              PIC X(1)     VALUE ';'.                       
031700     03 UT1H-KVANTMOT        PIC X(12)    VALUE 'QTY RECEIVED'.           
031800     03 FILLER              PIC X(1)     VALUE ';'.                       
031900     03 UT1H-KVSKROT         PIC X(12)    VALUE 'QTY SCRAPPED'.           
032000     03 FILLER              PIC X(1)     VALUE ';'.                       
032100     03 UT1H-PRAVCOST        PIC X(8)     VALUE 'AVG COST'.               
032200     03 FILLER              PIC X(1)     VALUE ';'.                       
032300     03 UT1H-PRAVCOST-OLD    PIC X(12)    VALUE 'OLD AVG COST'.           
032400     03 FILLER              PIC X(1)     VALUE ';'.                       
032500     03 UT1H-KVLS-OLD        PIC X(9)     VALUE 'OLD STOCK'.              
032600     03 FILLER              PIC X(1)     VALUE ';'.                       
032700     03 UT1H-FLSLUT          PIC X(12)    VALUE 'CLOSING FLAG'.           
032800     03 FILLER              PIC X(1)     VALUE ';'.                       
032900     03 UT1H-REMARKUP        PIC X(18)    VALUE                           
032910                                         'COST MARKUP FACTOR'.            
033000     03 FILLER              PIC X(1)     VALUE ';'.                       
033100     03 UT1H-IDKNOTNR        PIC X(14)    VALUE 'CREDIT NOTE NO'.         
033200     03 FILLER              PIC X(1)     VALUE ';'.                       
033300     03 UT1H-DAKRENOT        PIC X(16)    VALUE                           
033310                                         'CREDIT NOTE DATE'.              
033400     03 FILLER              PIC X(1)     VALUE ';'.                       
033500     03 UT1H-SUKREUTL       PIC X(15)    VALUE 'TOT CREDIT NOTE'.         
033600     03 FILLER              PIC X(1)     VALUE ';'.                       
033700     03 UT1H-SUKRENTO        PIC X(12)    VALUE 'SUM CREDITED'.           
033800     03 FILLER              PIC X(1)     VALUE ';'.                       
033900     03 UT1H-PRLANDCO        PIC X(9)     VALUE 'LAND COST'.              
034000     03 FILLER              PIC X(1)     VALUE ';'.                       
034100     03 UT1H-SUKRENOT        PIC X(18)    VALUE                           
034110                                            'TOTAL CREDIT NOTES'.         
034200     03 FILLER              PIC X(1)     vALUE ';'.                       
034300     03 UT1H-KVKREANT        PIC X(8)     VALUE 'CRED QTY'.               
034400     03 FILLER              PIC X(1)     VALUE ';'.                       
034500     03 UT1H-DARETILL        PIC X(17)    VALUE                           
034510                                         'DATE RETURNPERMIT'.             
034600     03 FILLER              PIC X(1)     VALUE ';'.                       
034700     03 UT1H-KVLEVANM        PIC X(19)    VALUE                           
034710                                         'QTY IN DISCR REPORT'.           
034800     03 FILLER              PIC X(1)     VALUE ';'.                       
034900     03 Ut1H-KVRETINL        PIC X(19)    VALUE                           
034910                                         'RECEIVED RETURN QTY'.           
035000     03 FILLER              PIC X(1)     VALUE ';'.                       
035100     03 UT1H-KVRETINL-SKR    PIC X(9)     VALUE 'SCRAP QTY'.              
035200     03 FILLER              PIC X(1)     VALUE ';'.                       
035300     03 UT1H-IDKONTO         PIC X(7)     VALUE 'ACCOUNT'.                
035400     03 FILLER              PIC X(1)     VALUE ';'.                       
035500     03 UT1H-IDKST           PIC X(11)    VALUE 'COST CENTRE'.            
035600     03 FILLER              PIC X(1)     VALUE ';'.                       
035700     03 UT1H-DAJUSTDA       PIC X(15)    VALUE 'ADJUSTMENT DATE'.         
035800     03 FILLER              PIC X(1)     VALUE ';'.                       
035900     03 UT1H-KVJUSTKV        PIC X(12)    VALUE 'ADJUSTED QTY'.           
036000     03 FILLER              PIC X(1)     VALUE ';'.                       
036100     03 UT1H-KDINVKAT        PIC X(20)    VALUE                           
036110                                         'STOCKTAKING CATEGORY'.          
036200     03 FILLER              PIC X(1)     VALUE ';'.                       
036300     03 UT1H-TEINVANM        PIC X(19)    VALUE                           
036310                                         'STOCKTAKING COMMENT'.           
036400     03 FILLER              PIC X(1)     VALUE ';'.                       
036500     03 UT1H-IDPRODNR        PIC X(13)    VALUE 'PRODUCTION NO'.          
036600     03 FILLER              PIC X(1)     VALUE ';'.                       
036700     03 UT1H-IDLEVNR         PIC X(8)     VALUE 'SUPPLIER'.               
036800     03 FILLER              PIC X(1)     VALUE ';'.                       
036900     03 UT1H-KVBEART         PIC X(11)    VALUE 'ORDERED QTY'.            
037000     03 FILLER              PIC X(1)     VALUE ';'.                       
037100     03 UT1H-DAORDDAT        PIC X(13)    VALUE 'ORDERING DATE'.          
037200     03 FILLER              PIC X(1)     VALUE ';'.                       
037300     03 UT1H-PRARTBEU        PIC X(11)    VALUE 'ORDER PRICE'.            
037400     03 FILLER              PIC X(1)     VALUE ';'.                       
037500     03 UT1H-PRARTNTO-GNB    PIC X(18)    VALUE                           
037510                                         'NET PRICE FROM GNB'.            
037600     03 FILLER              PIC X(1)     VALUE ';'.                       
037700     03 UT1H-IDFAKT-GNB      PIC X(18)    VALUE                           
037710                                         'INVOICING FROM GNB'.            
037800     03 FILLER              PIC X(1)     VALUE ';'.                       
037900     03 UT1H-DAFAKT-GNB      PIC X(18)    VALUE                           
037910                                         'GNB INVOICING DATE'.            
038000     03 FILLER              PIC X(1)     VALUE ';'.                       
038100     03 UT1H-IDFS           PIC X(15)    VALUE 'ADVICED NOTE NO'.         
038200     03 FILLER              PIC X(1)     VALUE ';'.                       
038300     03 UT1H-KVAVIS          PIC X(12)    VALUE 'NOTIFIED QTY'.           
038400     03 FILLER              PIC X(1)     VALUE ';'.                       
038500     03 UT1H-IDBYTRAP        PIC X(9)     VALUE 'REPORT NO'.              
038600     03 FILLER              PIC X(1)     VALUE ';'.                       
038700     03 UT1H-KVRETUR         PIC X(13)    VALUE 'QTY IN RETURN'.          
038800     03 FILLER              PIC X(1)     VALUE ';'.                       
038900     03 UT1H-SUAVCOST       PIC X(15)    VALUE 'SUM OF AVG COST'.         
039000     03 FILLER              PIC X(1)     VALUE ';'.                       
039100     03 UT1H-IDUSER          PIC X(7)     VALUE 'USER ID'.                
039200     03 FILLER              PIC X(1)     VALUE ';'.                       
039300     03 UT1H-KDAVCOST        PIC X(20)    VALUE                           
039310                                         'REASON CODE AVG COST'.          
039400     03 FILLER              PIC X(1)     VALUE ';'.                       
039500     03 UT1H-KDPSLLOC-NEW   PIC X(16)   VALUE 'NEW LOC PROD GRP'.         
039600     03 FILLER              PIC X(1)     VALUE ';'.                       
039700     03 UT1H-KDPSLLOC-OLD   PIC X(16)   VALUE 'OLD LOC PROD GRP'.         
039800     03 FILLER              PIC X(1)     VALUE ';'.                       
039900     03 UT1H-KVEFRS         PIC X(16)   VALUE 'NOT INVOICED QTY'.         
040000     03 FILLER          PIC X(1)     VALUE ';'.                           
040100     03 UT1H-KVLS            PIC X(13)    VALUE 'STOCK BALANCE'.          
040200     03 FILLER          PIC X(1)     VALUE ';'.                           
040300     03 UT1H-DAREGDAT        PIC X(9)     VALUE 'REGD DATE'.              
040400     03 FILLER          PIC X(1)     VALUE ';'.                           
040500     03 UT1H-DASTADAT        PIC X(10)    VALUE 'START DATE'.             
040600     03 FILLER              PIC X(1)     VALUE ';'.                       
040700     03 UT1H-PRKURS-SU       PIC X(21)    VALUE                           
040710                                         'CURR EXCHNAGE RATE SU'.         
040800     03 FILLER              PIC X(1)     VALUE ';'.                       
040900     03 UT1H-PRKURS-SC       PIC X(21)    VALUE                           
040910                                         'CURR EXCHNAGE RATE SC'.         
041000     03 FILLER              PIC X(1)     VALUE ';'.                       
041100     03 UT1H-PRKURS-UC       PIC X(21)    VALUE                           
041110                                         'CURR EXCHNAGE RATE UC'.         
041200     03 FILLER              PIC X(1)     VALUE ';'.                       
041300     03 UT1H-PRKURS-CU       PIC X(21)    VALUE                           
041310                                         'CURR EXCHNAGE RATE CU'.         
041400     03 FILLER              PIC X(1)     VALUE ';'.                       
041500                                                                          
041800 01  UT1-AREA.                                                            
041810     03 UT1-IDPTYP          PIC X(03)    VALUE SPACE.                     
041820     03 FILLER              PIC X(01)    VALUE ';'.                       
041830     03 UT1-KDEKOHT         PIC X(03)    VALUE SPACE.                     
041840     03 FILLER              PIC X(01)    VALUE ';'.                       
041850     03 UT1-IDFTG           PIC 9(02)    VALUE ZERO.                      
041860     03 FILLER              PIC X(01)    VALUE ';'.                       
041870     03 UT1-IDDC-SEND       PIC X(02)    VALUE SPACE.                     
041880     03 FILLER              PIC X(01)    VALUE ';'.                       
041890     03 UT1-IDDC-REC        PIC X(02)    VALUE SPACE.                     
041891     03 FILLER              PIC X(01)    VALUE ';'.                       
041892     03 UT1-IDDISTR         PIC 9(04)    VALUE ZERO.                      
041893     03 FILLER              PIC X(01)    VALUE ';'.                       
041894     03 UT1-IDKUNDNR        PIC 9(06)    VALUE ZERO.                      
041895     03 FILLER              PIC X(01)    VALUE ';'.                       
041896     03 UT1-IDFAKT          PIC 9(07)    VALUE ZERO.                      
041897     03 FILLER              PIC X(01)    VALUE ';'.                       
041898     03 UT1-KDFAKTYP        PIC X(01)    VALUE SPACE.                     
041899     03 FILLER              PIC X(01)    VALUE ';'.                       
041900     03 UT1-DAFAKT          PIC 9(08)    VALUE ZERO.                      
041901     03 FILLER              PIC X(01)    VALUE ';'.                       
041902     03 UT1-IDORDNR7        PIC 9(07)    VALUE ZERO.                      
041903     03 FILLER              PIC X(01)    VALUE ';'.                       
041904     03 UT1-IDARTNR         PIC 9(08)    VALUE ZERO.                      
041905     03 FILLER              PIC X(01)    VALUE ';'.                       
041906     03 UT1-KDPRODSL        PIC 9(02)    VALUE ZERO.                      
041907     03 FILLER              PIC X(01)    VALUE ';'.                       
041908     03 UT1-KDPSLLOC        PIC 9(02)    VALUE ZERO.                      
041909     03 FILLER              PIC X(01)    VALUE ';'.                       
041910     03 UT1-KVLEVART        PIC 9(07)    VALUE ZERO.                      
041911     03 FILLER              PIC X(01)    VALUE ';'.                       
041912     03 UT1-PRARTNTO        PIC Z(6)9.99 VALUE ZERO.                      
041915     03 FILLER              PIC X(01)    VALUE ';'.                       
041916     03 UT1-FLOVRLEV        PIC X(01)    VALUE SPACE.                     
041917     03 FILLER              PIC X(01)    VALUE ';'.                       
041918     03 UT1-KDFRAKT         PIC 9(02)    VALUE ZERO.                      
041919     03 FILLER              PIC X(01)    VALUE ';'.                       
041920     03 UT1-SUFAKTRE        PIC Z(7)9.99 VALUE ZERO.                      
041923     03 FILLER              PIC X(01)    VALUE ';'.                       
041924     03 UT1-PREMBHNT        PIC Z(6)9.99 VALUE ZERO.                      
041927     03 FILLER              PIC X(01)    VALUE ';'.                       
041928     03 UT1-PRFRAKT         PIC Z(6)9.99 VALUE ZERO.                      
041931     03 FILLER              PIC X(01)    VALUE ';'.                       
041932     03 UT1-PRFOERS         PIC Z(6)9.99 VALUE ZERO.                      
041935     03 FILLER              PIC X(01)    VALUE ';'.                       
041936     03 UT1-PRMOMS          PIC Z(6)9.99 VALUE ZERO.                      
041939     03 FILLER              PIC X(01)    VALUE ';'.                       
041940     03 UT1-PRLEGKST        PIC Z(6)9.99 VALUE ZERO.                      
041943     03 FILLER              PIC X(01)    VALUE ';'.                       
041944     03 UT1-SUFKTTILL       PIC Z(6)9.99 VALUE ZERO.                      
041947     03 FILLER              PIC X(01)    VALUE ';'.                       
041948     03 UT1-PRAVDRAG        PIC Z(6)9.99 VALUE ZERO.                      
041951     03 FILLER              PIC X(01)    VALUE ';'.                       
041952     03 UT1-SUFKTBEL        PIC Z(7)9.99 VALUE ZERO.                      
041955     03 FILLER              PIC X(01)    VALUE ';'.                       
041956     03 UT1-SUFKTUTL        PIC Z(10)9.99 VALUE ZERO.                     
041959     03 FILLER              PIC X(01)    VALUE ';'.                       
041960     03 UT1-PRKURS          PIC Z(5)9.99999 VALUE ZERO.                   
041963     03 FILLER              PIC X(01)    VALUE ';'.                       
041964     03 UT1-IDRAPPNR        PIC 9(07)    VALUE ZERO.                      
041965     03 FILLER              PIC X(01)    VALUE ';'.                       
041966     03 UT1-IDKOLLI         PIC 9(05)    VALUE ZERO.                      
041967     03 FILLER              PIC X(01)    VALUE ';'.                       
041968     03 UT1-KDANMORS        PIC X(02)    VALUE ZERO.                      
041969     03 FILLER              PIC X(01)    VALUE ';'.                       
041970     03 UT1-KDVALISO        PIC X(08)    VALUE SPACE.                     
041971     03 FILLER              PIC X(01)    VALUE ';'.                       
041972     03 UT1-DAINLINL        PIC 9(08)    VALUE ZERO.                      
041973     03 FILLER              PIC X(01)    VALUE ';'.                       
041974     03 UT1-KVANTMOT        PIC 9(07)    VALUE ZERO.                      
041975     03 FILLER              PIC X(01)    VALUE ';'.                       
041976     03 UT1-KVSKROT         PIC 9(07)    VALUE ZERO.                      
041977     03 FILLER              PIC X(01)    VALUE ';'.                       
041978     03 UT1-PRAVCOST        PIC Z(6)9.99 VALUE ZERO.                      
041981     03 FILLER              PIC X(01)    VALUE ';'.                       
041982     03 UT1-PRAVCOST-OLD    PIC Z(6)9.99 VALUE ZERO.                      
041985     03 FILLER              PIC X(01)    VALUE ';'.                       
041986     03 UT1-KVLS-OLD        PIC 9(07)    VALUE ZERO.                      
041987     03 FILLER              PIC X(01)    VALUE ';'.                       
041988     03 UT1-FLSLUT          PIC X(01)    VALUE SPACE.                     
041989     03 FILLER              PIC X(01)    VALUE ';'.                       
041990     03 UT1-REMARKUP        PIC Z(9).99   VALUE ZERO.                     
041993     03 FILLER              PIC X(01)    VALUE ';'.                       
041994     03 UT1-IDKNOTNR        PIC 9(07)    VALUE ZERO.                      
041995     03 FILLER              PIC X(01)    VALUE ';'.                       
041996     03 UT1-DAKRENOT        PIC 9(08)    VALUE ZERO.                      
041997     03 FILLER              PIC X(01)    VALUE ';'.                       
041998     03 UT1-SUKREUTL        PIC Z(10)9.99 VALUE ZERO.                     
042001     03 FILLER              PIC X(01)    VALUE ';'.                       
042002     03 UT1-SUKRENTO        PIC Z(10)9.99 VALUE ZERO.                     
042005     03 FILLER              PIC X(01)    VALUE ';'.                       
042006     03 UT1-PRLANDCO        PIC Z(5)9.99 VALUE ZERO.                      
042009     03 FILLER              PIC X(01)    VALUE ';'.                       
042010     03 UT1-SUKRENOT        PIC Z(6)9.99 VALUE ZERO.                      
042013     03 FILLER              PIC X(01)    VALUE ';'.                       
042014     03 UT1-KVKREANT        PIC 9(06)    VALUE ZERO.                      
042015     03 FILLER              PIC X(01)    VALUE ';'.                       
042016     03 UT1-DARETILL        PIC 9(08)    VALUE ZERO.                      
042017     03 FILLER              PIC X(01)    VALUE ';'.                       
042018     03 UT1-KVLEVANM        PIC 9(06)    VALUE ZERO.                      
042019     03 FILLER              PIC X(01)    VALUE ';'.                       
042020     03 UT1-KVRETINL        PIC 9(06)    VALUE ZERO.                      
042021     03 FILLER              PIC X(01)    VALUE ';'.                       
042022     03 UT1-KVRETINL-SKR    PIC 9(06)    VALUE ZERO.                      
042023     03 FILLER              PIC X(01)    VALUE ';'.                       
042024     03 UT1-IDKONTO         PIC 9(10)    VALUE ZERO.                      
042025     03 FILLER              PIC X(01)    VALUE ';'.                       
042026     03 UT1-IDKST           PIC X(10)    VALUE SPACE.                     
042027     03 FILLER              PIC X(01)    VALUE ';'.                       
042028     03 UT1-DAJUSTDA        PIC 9(08)    VALUE ZERO.                      
042029     03 FILLER              PIC X(01)    VALUE ';'.                       
042030     03 UT1-KVJUSTKV        PIC 9(7)     VALUE ZERO.                      
042031     03 FILLER              PIC X(01)    VALUE ';'.                       
042032     03 UT1-KDINVKAT        PIC 9(02)    VALUE ZERO.                      
042033     03 FILLER              PIC X(01)    VALUE ';'.                       
042034     03 UT1-TEINVANM        PIC X(25)    VALUE SPACE.                     
042035     03 FILLER              PIC X(01)    VALUE ';'.                       
042036     03 UT1-IDPRODNR        PIC 9(07)    VALUE ZERO.                      
042037     03 FILLER              PIC X(01)    VALUE ';'.                       
042038     03 UT1-IDLEVNR         PIC X(05)    VALUE SPACE.                     
042039     03 FILLER              PIC X(01)    VALUE ';'.                       
042040     03 UT1-KVBEART         PIC 9(06)    VALUE ZERO.                      
042041     03 FILLER              PIC X(01)    VALUE ';'.                       
042042     03 UT1-DAORDDAT        PIC 9(08)    VALUE ZERO.                      
042043     03 FILLER              PIC X(01)    VALUE ';'.                       
042044     03 UT1-PRARTBEU        PIC Z(4)9.99 VALUE ZERO.                      
042047     03 FILLER              PIC X(01)    VALUE ';'.                       
042048     03 UT1-PRARTNTO-GNB    PIC Z(6)9.99 VALUE ZERO.                      
042051     03 FILLER              PIC X(01)    VALUE ';'.                       
042052     03 UT1-IDFAKT-GNB      PIC X(08)    VALUE SPACE.                     
042053     03 FILLER              PIC X(01)    VALUE ';'.                       
042054     03 UT1-DAFAKT-GNB      PIC 9(08)    VALUE ZERO.                      
042055     03 FILLER              PIC X(01)    VALUE ';'.                       
042056     03 UT1-IDFS            PIC X(08)    VALUE SPACE.                     
042057     03 FILLER              PIC X(01)    VALUE ';'.                       
042058     03 UT1-KVAVIS          PIC 9(06)    VALUE ZERO.                      
042059     03 FILLER              PIC X(01)    VALUE ';'.                       
042060     03 UT1-IDBYTRAP        PIC 9(07)    VALUE ZERO.                      
042061     03 FILLER              PIC X(01)    VALUE ';'.                       
042062     03 UT1-KVRETUR         PIC 9(07)    VALUE ZERO.                      
042063     03 FILLER              PIC X(01)    VALUE ';'.                       
042064     03 UT1-SUAVCOST        PIC Z(8)9.99 VALUE ZERO.                      
042067     03 FILLER              PIC X(01)    VALUE ';'.                       
042068     03 UT1-IDUSER          PIC X(08)    VALUE SPACE.                     
042069     03 FILLER              PIC X(01)    VALUE ';'.                       
042070     03 UT1-KDAVCOST        PIC X(02)    VALUE SPACE.                     
042071     03 FILLER              PIC X(01)    VALUE ';'.                       
042072     03 UT1-KDPSLLOC-NEW    PIC 9(02)    VALUE ZERO.                      
042073     03 FILLER              PIC X(01)    VALUE ';'.                       
042074     03 UT1-KDPSLLOC-OLD    PIC 9(02)    VALUE ZERO.                      
042075     03 FILLER              PIC X(01)    VALUE ';'.                       
042076     03 UT1-KVEFRS          PIC 9(7)     VALUE ZERO.                      
042077     03 FILLER              PIC X(01)    VALUE ';'.                       
042078     03 UT1-KVLS            PIC 9(7)     VALUE ZERO.                      
042079     03 FILLER              PIC X(01)    VALUE ';'.                       
042080     03 UT1-DAREGDAT        PIC 9(08)    VALUE ZERO.                      
042081     03 FILLER              PIC X(01)    VALUE ';'.                       
042082     03 UT1-DASTADAT        PIC 9(08)    VALUE ZERO.                      
042083     03 FILLER              PIC X(01)    VALUE ';'.                       
042084     03 UT1-PRKURS-SU       PIC Z(5)9.99999 VALUE ZERO.                   
042087     03 FILLER              PIC X(01)    VALUE ';'.                       
042088     03 UT1-PRKURS-SC       PIC Z(5)9.99999 VALUE ZERO.                   
042091     03 FILLER              PIC X(01)    VALUE ';'.                       
042092     03 UT1-PRKURS-UC       PIC Z(5)9.99999 VALUE ZERO.                   
042095     03 FILLER              PIC X(01)    VALUE ';'.                       
042096     03 UT1-PRKURS-CU       PIC Z(5)9.99999 VALUE ZERO.                   
042099     03 FILLER              PIC X(01)    VALUE ';'.                       
042100                                                                          
042101 01 UT2-HEADER.                                                           
042102     03 UT2H-IDPTYP          PIC X(11)    VALUE 'RECORD TYPE'.            
042103     03 FILLER              PIC X(1)     VALUE ';'.                       
042104     03 UT2H-KDEKOHT         PIC X(14)    VALUE 'ECONOMIC EVENT'.         
042105     03 FILLER              PIC X(1)     VALUE ';'.                       
042106     03 UT2H-IDFTG           PIC X(13)    VALUE 'COMPANY ID NO'.          
042107     03 FILLER              PIC X(1)     VALUE ';'.                       
042108     03 UT2H-IDDC-SEND       PIC X(7)     VALUE 'SEND DC'.                
042109     03 FILLER              PIC X(1)     VALUE ';'.                       
042110     03 UT2H-IDDC-REC        PIC X(6)     VALUE 'REC DC'.                 
042111     03 FILLER              PIC X(1)     VALUE ';'.                       
042112     03 UT2H-IDDISTR         PIC X(8)     VALUE 'DISTRICT'.               
042113     03 FILLER              PIC X(1)     VALUE ';'.                       
042114     03 UT2H-IDKUNDNR        PIC X(8)     VALUE 'CUSTOMER'.               
042115     03 FILLER              PIC X(1)     VALUE ';'.                       
042116     03 UT2H-IDFAKT          PIC X(6)     VALUE 'INV NO'.                 
042117     03 FILLER              PIC X(1)     VALUE ';'.                       
042118     03 UT2H-KDFAKTYP        PIC X(8)     VALUE 'INV TYPE'.               
042119     03 FILLER              PIC X(1)     VALUE ';'.                       
042120     03 UT2H-DAFAKT          PIC X(8)     VALUE 'INV DATE'.               
042121     03 FILLER              PIC X(1)     VALUE ';'.                       
042122     03 UT2H-IDORDNR7        PIC X(6)     VALUE 'ORD NO'.                 
042123     03 FILLER              PIC X(1)     VALUE ';'.                       
042124     03 UT2H-IDARTNR         PIC X(7)     VALUE 'PART NO'.                
042125     03 FILLER              PIC X(1)     VALUE ';'.                       
042126     03 UT2H-KDPRODSL        PIC X(8)     VALUE 'PROD GRP'.               
042127     03 FILLER              PIC X(1)     VALUE ';'.                       
042128     03 UT2H-KDPSLLOC        PIC X(12)    VALUE 'LOC PROD GRP'.           
042129     03 FILLER              PIC X(1)     VALUE ';'.                       
042130     03 UT2H-KVLEVART        PIC X(3)     VALUE 'QTY'.                    
042131     03 FILLER              PIC X(1)     VALUE ';'.                       
042132     03 UT2H-PRARTNTO        PIC X(9)     VALUE 'NET PRICE'.              
042133     03 FILLER              PIC X(1)     VALUE ';'.                       
042134     03 UT2H-FLOVRLEV        PIC X(13)    VALUE 'OVER DELIVERY'.          
042135     03 FILLER              PIC X(1)     VALUE ';'.                       
042136     03 UT2H-KDFRAKT         PIC X(12)    VALUE 'FREIGHT CODE'.           
042137     03 FILLER              PIC X(1)     VALUE ';'.                       
042138     03 UT2H-SUFAKTRE        PIC X(17)    VALUE                           
042139                                         'SALES SPARE PARTS'.             
042140     03 FILLER              PIC X(1)     VALUE ';'.                       
042141     03 UT2H-PREMBHNT        PIC X(20)    VALUE                           
042142                                         'PACKING & HANDL COST'.          
042143     03 FILLER              PIC X(1)     VALUE ';'.                       
042144     03 UT2H-PRFRAKT         PIC X(12)    VALUE 'FREIGHT COST'.           
042145     03 FILLER              PIC X(1)     VALUE ';'.                       
042146     03 UT2H-PRFOERS         PIC X(9)     VALUE 'INSURANCE'.              
042147     03 FILLER              PIC X(1)     VALUE ';'.                       
042148     03 UT2H-PRMOMS          PIC X(3)     VALUE 'VAT'.                    
042149     03 FILLER              PIC X(1)     VALUE ';'.                       
042150     03 UT2H-PRLEGKST        PIC X(10)    VALUE 'LEGAL COST'.             
042151     03 FILLER              PIC X(1)     VALUE ';'.                       
042152     03 UT2H-SUFKTTILL      PIC X(15)    VALUE 'ADDITIONAL COST'.         
042153     03 FILLER              PIC X(1)     VALUE ';'.                       
042154     03 UT2H-PRAVDRAG        PIC X(13)    VALUE 'DEDUCTION INV'.          
042155     03 FILLER              PIC X(1)     VALUE ';'.                       
042156     03 UT2H-SUFKTBEL        PIC X(17)    VALUE                           
042157                                         'TOT INVOICED AMNT'.             
042158     03 FILLER              PIC X(1)     VALUE ';'.                       
042159     03 UT2H-SUFKTUTL        PIC X(7)     VALUE 'INV SUM'.                
042160     03 FILLER              PIC X(1)     VALUE ';'.                       
042161     03 UT2H-PRKURS          PIC X(18)    VALUE                           
042162                                         'CURR EXCHANGE RATE'.            
042163     03 FILLER              PIC X(1)     VALUE ';'.                       
042164     03 UT2H-IDRAPPNR        PIC X(18)    VALUE                           
042165                                         'DISCREPANCY RPT NO'.            
042166     03 FILLER              PIC X(1)     VALUE ';'.                       
042167     03 UT2H-IDKOLLI         PIC X(7)     VALUE 'CASE NO'.                
042168     03 FILLER              PIC X(1)     VALUE ';'.                       
042169     03 UT2H-KDANMORS        PIC X(14)    VALUE 'DISCR RPT CODE'.         
042170     03 FILLER              PIC X(1)     VALUE ';'.                       
042171     03 UT2H-KDVALISO        PIC X(9)     VALUE 'CURR CODE'.              
042172     03 FILLER              PIC X(1)     VALUE ';'.                       
042173     03 UT2H-DAINLINL        PIC X(14)    VALUE 'REPORTING DATE'.         
042174     03 FILLER              PIC X(1)     VALUE ';'.                       
042175     03 UT2H-KVANTMOT        PIC X(12)    VALUE 'QTY RECEIVED'.           
042176     03 FILLER              PIC X(1)     VALUE ';'.                       
042177     03 UT2H-KVSKROT         PIC X(12)    VALUE 'QTY SCRAPPED'.           
042178     03 FILLER              PIC X(1)     VALUE ';'.                       
042179     03 UT2H-PRAVCOST        PIC X(8)     VALUE 'AVG COST'.               
042180     03 FILLER              PIC X(1)     VALUE ';'.                       
042181     03 UT2H-PRAVCOST-OLD    PIC X(12)    VALUE 'OLD AVG COST'.           
042182     03 FILLER              PIC X(1)     VALUE ';'.                       
042183     03 UT2H-KVLS-OLD        PIC X(9)     VALUE 'OLD STOCK'.              
042184     03 FILLER              PIC X(1)     VALUE ';'.                       
042185     03 UT2H-FLSLUT          PIC X(12)    VALUE 'CLOSING FLAG'.           
042186     03 FILLER              PIC X(1)     VALUE ';'.                       
042187     03 UT2H-REMARKUP        PIC X(18)    VALUE                           
042188                                         'COST MARKUP FACTOR'.            
042189     03 FILLER              PIC X(1)     VALUE ';'.                       
042190     03 UT2H-IDKNOTNR        PIC X(14)    VALUE 'CREDIT NOTE NO'.         
042191     03 FILLER              PIC X(1)     VALUE ';'.                       
042192     03 UT2H-DAKRENOT        PIC X(16)    VALUE                           
042193                                         'CREDIT NOTE DATE'.              
042194     03 FILLER              PIC X(1)     VALUE ';'.                       
042195     03 UT2H-SUKREUTL       PIC X(15)    VALUE 'TOT CREDIT NOTE'.         
042196     03 FILLER              PIC X(1)     VALUE ';'.                       
042197     03 UT2H-SUKRENTO        PIC X(12)    VALUE 'SUM CREDITED'.           
042198     03 FILLER              PIC X(1)     VALUE ';'.                       
042199     03 UT2H-PRLANDCO        PIC X(9)     VALUE 'LAND COST'.              
042200     03 FILLER              PIC X(1)     VALUE ';'.                       
042201     03 UT2H-SUKRENOT        PIC X(18)    VALUE                           
042202                                            'TOTAL CREDIT NOTES'.         
042203     03 FILLER              PIC X(1)     vALUE ';'.                       
042204     03 UT2H-KVKREANT        PIC X(8)     VALUE 'CRED QTY'.               
042205     03 FILLER              PIC X(1)     VALUE ';'.                       
042206     03 UT2H-DARETILL        PIC X(17)    VALUE                           
042207                                         'DATE RETURNPERMIT'.             
042208     03 FILLER              PIC X(1)     VALUE ';'.                       
042209     03 UT2H-KVLEVANM        PIC X(19)    VALUE                           
042210                                         'QTY IN DISCR REPORT'.           
042211     03 FILLER              PIC X(1)     VALUE ';'.                       
042212     03 UT2H-KVRETINL        PIC X(19)    VALUE                           
042213                                         'RECEIVED RETURN QTY'.           
042214     03 FILLER              PIC X(1)     VALUE ';'.                       
042215     03 UT2H-KVRETINL-SKR    PIC X(9)     VALUE 'SCRAP QTY'.              
042216     03 FILLER              PIC X(1)     VALUE ';'.                       
042217     03 UT2H-IDKONTO         PIC X(7)     VALUE 'ACCOUNT'.                
042218     03 FILLER              PIC X(1)     VALUE ';'.                       
042219     03 UT2H-IDKST           PIC X(11)    VALUE 'COST CENTRE'.            
042220     03 FILLER              PIC X(1)     VALUE ';'.                       
042221     03 UT2H-DAJUSTDA       PIC X(15)    VALUE 'ADJUSTMENT DATE'.         
042222     03 FILLER              PIC X(1)     VALUE ';'.                       
042223     03 UT2H-KVJUSTKV        PIC X(12)    VALUE 'ADJUSTED QTY'.           
042224     03 FILLER              PIC X(1)     VALUE ';'.                       
042225     03 UT2H-KDINVKAT        PIC X(20)    VALUE                           
042226                                         'STOCKTAKING CATEGORY'.          
042227     03 FILLER              PIC X(1)     VALUE ';'.                       
042228     03 UT2H-TEINVANM        PIC X(19)    VALUE                           
042229                                         'STOCKTAKING COMMENT'.           
042230     03 FILLER              PIC X(1)     VALUE ';'.                       
042231     03 UT2H-IDPRODNR        PIC X(13)    VALUE 'PRODUCTION NO'.          
042232     03 FILLER              PIC X(1)     VALUE ';'.                       
042233     03 UT2H-IDLEVNR         PIC X(8)     VALUE 'SUPPLIER'.               
042234     03 FILLER              PIC X(1)     VALUE ';'.                       
042235     03 UT2H-KVBEART         PIC X(11)    VALUE 'ORDERED QTY'.            
042236     03 FILLER              PIC X(1)     VALUE ';'.                       
042237     03 UT2H-DAORDDAT        PIC X(13)    VALUE 'ORDERING DATE'.          
042238     03 FILLER              PIC X(1)     VALUE ';'.                       
042239     03 UT2H-PRARTBEU        PIC X(11)    VALUE 'ORDER PRICE'.            
042240     03 FILLER              PIC X(1)     VALUE ';'.                       
042241     03 UT2H-PRARTNTO-GNB    PIC X(18)    VALUE                           
042242                                         'NET PRICE FROM GNB'.            
042243     03 FILLER              PIC X(1)     VALUE ';'.                       
042244     03 UT2H-IDFAKT-GNB      PIC X(18)    VALUE                           
042245                                         'INVOICING FROM GNB'.            
042246     03 FILLER              PIC X(1)     VALUE ';'.                       
042247     03 UT2H-DAFAKT-GNB      PIC X(18)    VALUE                           
042248                                         'GNB INVOICING DATE'.            
042249     03 FILLER              PIC X(1)     VALUE ';'.                       
042250     03 UT2H-IDFS           PIC X(15)    VALUE 'ADVICED NOTE NO'.         
042251     03 FILLER              PIC X(1)     VALUE ';'.                       
042252     03 UT2H-KVAVIS          PIC X(12)    VALUE 'NOTIFIED QTY'.           
042253     03 FILLER              PIC X(1)     VALUE ';'.                       
042254     03 UT2H-IDBYTRAP        PIC X(9)     VALUE 'REPORT NO'.              
042255     03 FILLER              PIC X(1)     VALUE ';'.                       
042256     03 UT2H-KVRETUR         PIC X(13)    VALUE 'QTY IN RETURN'.          
042257     03 FILLER              PIC X(1)     VALUE ';'.                       
042258     03 UT2H-SUAVCOST       PIC X(15)    VALUE 'SUM OF AVG COST'.         
042259     03 FILLER              PIC X(1)     VALUE ';'.                       
042260     03 UT2H-IDUSER          PIC X(7)     VALUE 'USER ID'.                
042261     03 FILLER              PIC X(1)     VALUE ';'.                       
042262     03 UT2H-KDAVCOST        PIC X(20)    VALUE                           
042263                                         'REASON CODE AVG COST'.          
042264     03 FILLER              PIC X(1)     VALUE ';'.                       
042265     03 UT2H-KDPSLLOC-NEW   PIC X(16)   VALUE 'NEW LOC PROD GRP'.         
042266     03 FILLER              PIC X(1)     VALUE ';'.                       
042267     03 UT2H-KDPSLLOC-OLD   PIC X(16)   VALUE 'OLD LOC PROD GRP'.         
042268     03 FILLER              PIC X(1)     VALUE ';'.                       
042269     03 UT2H-KVEFRS         PIC X(16)   VALUE 'NOT INVOICED QTY'.         
042270     03 FILLER          PIC X(1)     VALUE ';'.                           
042271     03 UT2H-KVLS            PIC X(13)    VALUE 'STOCK BALANCE'.          
042272     03 FILLER          PIC X(1)     VALUE ';'.                           
042273     03 UT2H-DAREGDAT        PIC X(9)     VALUE 'REGD DATE'.              
042274     03 FILLER          PIC X(1)     VALUE ';'.                           
042275     03 UT2H-DASTADAT        PIC X(10)    VALUE 'START DATE'.             
042276     03 FILLER              PIC X(1)     VALUE ';'.                       
042277     03 UT2H-PRKURS-SU       PIC X(21)    VALUE                           
042278                                         'CURR EXCHNAGE RATE SU'.         
042279     03 FILLER              PIC X(1)     VALUE ';'.                       
042280     03 UT2H-PRKURS-SC       PIC X(21)    VALUE                           
042281                                         'CURR EXCHNAGE RATE SC'.         
042282     03 FILLER              PIC X(1)     VALUE ';'.                       
042283     03 UT2H-PRKURS-UC       PIC X(21)    VALUE                           
042284                                         'CURR EXCHNAGE RATE UC'.         
042285     03 FILLER              PIC X(1)     VALUE ';'.                       
042286     03 UT2H-PRKURS-CU       PIC X(21)    VALUE                           
042287                                         'CURR EXCHNAGE RATE CU'.         
042290     03 FILLER              PIC X(1)     VALUE ';'.                       
042291                                                                          
042292 01  UT2-AREA.                                                            
042293     03 UT2-IDPTYP          PIC X(03)    VALUE SPACE.                     
042294     03 FILLER              PIC X(01)    VALUE ';'.                       
042295     03 UT2-KDEKOHT         PIC X(03)    VALUE SPACE.                     
042296     03 FILLER              PIC X(01)    VALUE ';'.                       
042297     03 UT2-IDFTG           PIC 9(02)    VALUE ZERO.                      
042298     03 FILLER              PIC X(01)    VALUE ';'.                       
042299     03 UT2-IDDC-SEND       PIC X(02)    VALUE SPACE.                     
042300     03 FILLER              PIC X(01)    VALUE ';'.                       
042310     03 UT2-IDDC-REC        PIC X(02)    VALUE SPACE.                     
042320     03 FILLER              PIC X(01)    VALUE ';'.                       
042330     03 UT2-IDDISTR         PIC 9(04)    VALUE ZERO.                      
042340     03 FILLER              PIC X(01)    VALUE ';'.                       
042350     03 UT2-IDKUNDNR        PIC 9(06)    VALUE ZERO.                      
042360     03 FILLER              PIC X(01)    VALUE ';'.                       
042370     03 UT2-IDFAKT          PIC 9(07)    VALUE ZERO.                      
042380     03 FILLER              PIC X(01)    VALUE ';'.                       
042390     03 UT2-KDFAKTYP        PIC X(01)    VALUE SPACE.                     
042391     03 FILLER              PIC X(01)    VALUE ';'.                       
042392     03 UT2-DAFAKT          PIC 9(08)    VALUE ZERO.                      
042393     03 FILLER              PIC X(01)    VALUE ';'.                       
042394     03 UT2-IDORDNR7        PIC 9(07)    VALUE ZERO.                      
042395     03 FILLER              PIC X(01)    VALUE ';'.                       
042396     03 UT2-IDARTNR         PIC 9(08)    VALUE ZERO.                      
042397     03 FILLER              PIC X(01)    VALUE ';'.                       
042398     03 UT2-KDPRODSL        PIC 9(02)    VALUE ZERO.                      
042399     03 FILLER              PIC X(01)    VALUE ';'.                       
042400     03 UT2-KDPSLLOC        PIC 9(02)    VALUE ZERO.                      
042410     03 FILLER              PIC X(01)    VALUE ';'.                       
042420     03 UT2-KVLEVART        PIC 9(07)    VALUE ZERO.                      
042430     03 FILLER              PIC X(01)    VALUE ';'.                       
042440     03 UT2-PRARTNTO        PIC Z(6)9.99 VALUE ZERO.                      
042450     03 FILLER              PIC X(01)    VALUE ';'.                       
042460     03 UT2-FLOVRLEV        PIC X(01)    VALUE SPACE.                     
042470     03 FILLER              PIC X(01)    VALUE ';'.                       
042480     03 UT2-KDFRAKT         PIC 9(02)    VALUE ZERO.                      
042490     03 FILLER              PIC X(01)    VALUE ';'.                       
042491     03 UT2-SUFAKTRE        PIC Z(7)9.99 VALUE ZERO.                      
042492     03 FILLER              PIC X(01)    VALUE ';'.                       
042493     03 UT2-PREMBHNT        PIC Z(6)9.99 VALUE ZERO.                      
042494     03 FILLER              PIC X(01)    VALUE ';'.                       
042495     03 UT2-PRFRAKT         PIC Z(6)9.99 VALUE ZERO.                      
042496     03 FILLER              PIC X(01)    VALUE ';'.                       
042497     03 UT2-PRFOERS         PIC Z(6)9.99 VALUE ZERO.                      
042498     03 FILLER              PIC X(01)    VALUE ';'.                       
042499     03 UT2-PRMOMS          PIC Z(6)9.99 VALUE ZERO.                      
042500     03 FILLER              PIC X(01)    VALUE ';'.                       
042510     03 UT2-PRLEGKST        PIC Z(6)9.99 VALUE ZERO.                      
042520     03 FILLER              PIC X(01)    VALUE ';'.                       
042530     03 UT2-SUFKTTILL       PIC Z(6)9.99 VALUE ZERO.                      
042540     03 FILLER              PIC X(01)    VALUE ';'.                       
042550     03 UT2-PRAVDRAG        PIC Z(6)9.99 VALUE ZERO.                      
042560     03 FILLER              PIC X(01)    VALUE ';'.                       
042570     03 UT2-SUFKTBEL        PIC Z(7)9.99 VALUE ZERO.                      
042580     03 FILLER              PIC X(01)    VALUE ';'.                       
042590     03 UT2-SUFKTUTL        PIC Z(10)9.99 VALUE ZERO.                     
042591     03 FILLER              PIC X(01)    VALUE ';'.                       
042592     03 UT2-PRKURS          PIC Z(5)9.99999 VALUE ZERO.                   
042593     03 FILLER              PIC X(01)    VALUE ';'.                       
042594     03 UT2-IDRAPPNR        PIC 9(07)    VALUE ZERO.                      
042595     03 FILLER              PIC X(01)    VALUE ';'.                       
042596     03 UT2-IDKOLLI         PIC 9(05)    VALUE ZERO.                      
042597     03 FILLER              PIC X(01)    VALUE ';'.                       
042598     03 UT2-KDANMORS        PIC X(02)    VALUE ZERO.                      
042599     03 FILLER              PIC X(01)    VALUE ';'.                       
042600     03 UT2-KDVALISO        PIC X(08)    VALUE SPACE.                     
042610     03 FILLER              PIC X(01)    VALUE ';'.                       
042620     03 UT2-DAINLINL        PIC 9(08)    VALUE ZERO.                      
042630     03 FILLER              PIC X(01)    VALUE ';'.                       
042640     03 UT2-KVANTMOT        PIC 9(07)    VALUE ZERO.                      
042650     03 FILLER              PIC X(01)    VALUE ';'.                       
042660     03 UT2-KVSKROT         PIC 9(07)    VALUE ZERO.                      
042670     03 FILLER              PIC X(01)    VALUE ';'.                       
042680     03 UT2-PRAVCOST        PIC Z(6)9.99 VALUE ZERO.                      
042690     03 FILLER              PIC X(01)    VALUE ';'.                       
042691     03 UT2-PRAVCOST-OLD    PIC Z(6)9.99 VALUE ZERO.                      
042692     03 FILLER              PIC X(01)    VALUE ';'.                       
042693     03 UT2-KVLS-OLD        PIC 9(07)    VALUE ZERO.                      
042694     03 FILLER              PIC X(01)    VALUE ';'.                       
042695     03 UT2-FLSLUT          PIC X(01)    VALUE SPACE.                     
042696     03 FILLER              PIC X(01)    VALUE ';'.                       
042697     03 UT2-REMARKUP        PIC Z(9).99   VALUE ZERO.                     
042698     03 FILLER              PIC X(01)    VALUE ';'.                       
042699     03 UT2-IDKNOTNR        PIC 9(07)    VALUE ZERO.                      
042700     03 FILLER              PIC X(01)    VALUE ';'.                       
042710     03 UT2-DAKRENOT        PIC 9(08)    VALUE ZERO.                      
042720     03 FILLER              PIC X(01)    VALUE ';'.                       
042730     03 UT2-SUKREUTL        PIC Z(10)9.99 VALUE ZERO.                     
042740     03 FILLER              PIC X(01)    VALUE ';'.                       
042750     03 UT2-SUKRENTO        PIC Z(10)9.99 VALUE ZERO.                     
042760     03 FILLER              PIC X(01)    VALUE ';'.                       
042770     03 UT2-PRLANDCO        PIC Z(5)9.99 VALUE ZERO.                      
042780     03 FILLER              PIC X(01)    VALUE ';'.                       
042790     03 UT2-SUKRENOT        PIC Z(6)9.99 VALUE ZERO.                      
042791     03 FILLER              PIC X(01)    VALUE ';'.                       
042792     03 UT2-KVKREANT        PIC 9(06)    VALUE ZERO.                      
042793     03 FILLER              PIC X(01)    VALUE ';'.                       
042794     03 UT2-DARETILL        PIC 9(08)    VALUE ZERO.                      
042795     03 FILLER              PIC X(01)    VALUE ';'.                       
042796     03 UT2-KVLEVANM        PIC 9(06)    VALUE ZERO.                      
042797     03 FILLER              PIC X(01)    VALUE ';'.                       
042798     03 UT2-KVRETINL        PIC 9(06)    VALUE ZERO.                      
042799     03 FILLER              PIC X(01)    VALUE ';'.                       
042800     03 UT2-KVRETINL-SKR    PIC 9(06)    VALUE ZERO.                      
042810     03 FILLER              PIC X(01)    VALUE ';'.                       
042820     03 UT2-IDKONTO         PIC 9(10)    VALUE ZERO.                      
042830     03 FILLER              PIC X(01)    VALUE ';'.                       
042840     03 UT2-IDKST           PIC X(10)    VALUE SPACE.                     
042850     03 FILLER              PIC X(01)    VALUE ';'.                       
042860     03 UT2-DAJUSTDA        PIC 9(08)    VALUE ZERO.                      
042870     03 FILLER              PIC X(01)    VALUE ';'.                       
042880     03 UT2-KVJUSTKV        PIC 9(7)     VALUE ZERO.                      
042890     03 FILLER              PIC X(01)    VALUE ';'.                       
042891     03 UT2-KDINVKAT        PIC 9(02)    VALUE ZERO.                      
042892     03 FILLER              PIC X(01)    VALUE ';'.                       
042893     03 UT2-TEINVANM        PIC X(25)    VALUE SPACE.                     
042894     03 FILLER              PIC X(01)    VALUE ';'.                       
042895     03 UT2-IDPRODNR        PIC 9(07)    VALUE ZERO.                      
042896     03 FILLER              PIC X(01)    VALUE ';'.                       
042897     03 UT2-IDLEVNR         PIC X(05)    VALUE SPACE.                     
042898     03 FILLER              PIC X(01)    VALUE ';'.                       
042899     03 UT2-KVBEART         PIC 9(06)    VALUE ZERO.                      
042900     03 FILLER              PIC X(01)    VALUE ';'.                       
042910     03 UT2-DAORDDAT        PIC 9(08)    VALUE ZERO.                      
042920     03 FILLER              PIC X(01)    VALUE ';'.                       
042930     03 UT2-PRARTBEU        PIC Z(4)9.99 VALUE ZERO.                      
042940     03 FILLER              PIC X(01)    VALUE ';'.                       
042950     03 UT2-PRARTNTO-GNB    PIC Z(6)9.99 VALUE ZERO.                      
042960     03 FILLER              PIC X(01)    VALUE ';'.                       
042970     03 UT2-IDFAKT-GNB      PIC X(08)    VALUE SPACE.                     
042980     03 FILLER              PIC X(01)    VALUE ';'.                       
042990     03 UT2-DAFAKT-GNB      PIC 9(08)    VALUE ZERO.                      
042991     03 FILLER              PIC X(01)    VALUE ';'.                       
042992     03 UT2-IDFS            PIC X(08)    VALUE SPACE.                     
042993     03 FILLER              PIC X(01)    VALUE ';'.                       
042994     03 UT2-KVAVIS          PIC 9(06)    VALUE ZERO.                      
042995     03 FILLER              PIC X(01)    VALUE ';'.                       
042996     03 UT2-IDBYTRAP        PIC 9(07)    VALUE ZERO.                      
042997     03 FILLER              PIC X(01)    VALUE ';'.                       
042998     03 UT2-KVRETUR         PIC 9(07)    VALUE ZERO.                      
042999     03 FILLER              PIC X(01)    VALUE ';'.                       
043000     03 UT2-SUAVCOST        PIC Z(8)9.99 VALUE ZERO.                      
043010     03 FILLER              PIC X(01)    VALUE ';'.                       
043020     03 UT2-IDUSER          PIC X(08)    VALUE SPACE.                     
043030     03 FILLER              PIC X(01)    VALUE ';'.                       
043040     03 UT2-KDAVCOST        PIC X(02)    VALUE SPACE.                     
043050     03 FILLER              PIC X(01)    VALUE ';'.                       
043060     03 UT2-KDPSLLOC-NEW    PIC 9(02)    VALUE ZERO.                      
043070     03 FILLER              PIC X(01)    VALUE ';'.                       
043080     03 UT2-KDPSLLOC-OLD    PIC 9(02)    VALUE ZERO.                      
043090     03 FILLER              PIC X(01)    VALUE ';'.                       
043091     03 UT2-KVEFRS          PIC 9(7)     VALUE ZERO.                      
043092     03 FILLER              PIC X(01)    VALUE ';'.                       
043093     03 UT2-KVLS            PIC 9(7)     VALUE ZERO.                      
043094     03 FILLER              PIC X(01)    VALUE ';'.                       
043095     03 UT2-DAREGDAT        PIC 9(08)    VALUE ZERO.                      
043096     03 FILLER              PIC X(01)    VALUE ';'.                       
043097     03 UT2-DASTADAT        PIC 9(08)    VALUE ZERO.                      
043098     03 FILLER              PIC X(01)    VALUE ';'.                       
043099     03 UT2-PRKURS-SU       PIC Z(5)9.99999 VALUE ZERO.                   
043100     03 FILLER              PIC X(01)    VALUE ';'.                       
043110     03 UT2-PRKURS-SC       PIC Z(5)9.99999 VALUE ZERO.                   
043120     03 FILLER              PIC X(01)    VALUE ';'.                       
043130     03 UT2-PRKURS-UC       PIC Z(5)9.99999 VALUE ZERO.                   
043140     03 FILLER              PIC X(01)    VALUE ';'.                       
043150     03 UT2-PRKURS-CU       PIC Z(5)9.99999 VALUE ZERO.                   
043160     03 FILLER              PIC X(01)    VALUE ';'.                       
043170                                                                          
043200     EJECT                                                                
058700 PROCEDURE DIVISION.                                                      
058800 MAIN SECTION.                                                            
058900     SKIP2                                                                
059000                                                                          
059100     PERFORM A-INIT                                                       
059200     PERFORM S01-READ-W56022                                              
059300     PERFORM UNTIL W56022-EOF-SW = YES                                    
059400       PERFORM B-MOVE-DATA                                                
059500       PERFORM S01-READ-W56022                                            
059600     END-PERFORM                                                          
059610                                                                          
059700     PERFORM S02-READ-W56023                                              
059800     PERFORM UNTIL W56023-EOF-SW = YES                                    
059900       PERFORM C-MOVE-DATA                                                
060000       PERFORM S02-READ-W56023                                            
060100     END-PERFORM                                                          
060200                                                                          
060300     PERFORM Z-FINIT                                                      
060400                                                                          
060500     MOVE ZERO TO RETURN-CODE                                             
060600     GOBACK                                                               
060700     .                                                                    
060800     EJECT                                                                
060900 A-INIT SECTION.                                                          
061000                                                                          
061100     OPEN INPUT  W56022                                                   
061200                 W56023                                                   
061300                                                                          
061400     OPEN OUTPUT W56022A                                                  
061500                 W56023A                                                  
061501                                                                          
061510     PERFORM S11-WRITE-HEADERS                                            
061700     ACCEPT TODAYS-DATE  FROM DATE                                        
061800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
061900     .                                                                    
062000     EJECT                                                                
062100 B-MOVE-DATA SECTION.                                                     
062200     IF IN1-IDPTYP = 'AX2'                                                
062300       MOVE 1AX2-IDPTYP      TO  UT1-IDPTYP                               
062400       MOVE 1AX2-KDEKOHT     TO  UT1-KDEKOHT                              
062500       MOVE 1AX2-IDFTG       TO  UT1-IDFTG                                
062600       MOVE 1AX2-IDDC-SEND   TO  UT1-IDDC-SEND                            
062700       MOVE 1AX2-IDDC-REC    TO  UT1-IDDC-REC                             
062800       MOVE 1AX2-IDDISTR     TO  UT1-IDDISTR                              
062900       MOVE 1AX2-IDKUNDNR    TO  UT1-IDKUNDNR                             
063000       MOVE 1AX2-IDFAKT      TO  UT1-IDFAKT                               
063100       MOVE 1AX2-KDFAKTYP    TO  UT1-KDFAKTYP                             
063200       MOVE 1AX2-DAFAKT      TO  UT1-DAFAKT                               
063300       MOVE 1AX2-IDORDNR7    TO  UT1-IDORDNR7                             
063400       MOVE 1AX2-IDARTNR     TO  UT1-IDARTNR                              
063500       MOVE 1AX2-KDPRODSL    TO  UT1-KDPRODSL                             
063600       MOVE 1AX2-KDPSLLOC    TO  UT1-KDPSLLOC                             
063700       MOVE 1AX2-KVLEVART    TO  UT1-KVLEVART                             
063800       MOVE 1AX2-PRARTNTO    TO  UT1-PRARTNTO                             
063900       MOVE 1AX2-FLOVRLEV    TO  UT1-FLOVRLEV                             
064000       MOVE ZERO             TO  UT1-KDFRAKT                              
064100       MOVE ZERO             TO  UT1-SUFAKTRE                             
064200       MOVE ZERO             TO  UT1-PREMBHNT                             
064300       MOVE ZERO             TO  UT1-PRFRAKT                              
064400       MOVE ZERO             TO  UT1-PRFOERS                              
064500       MOVE ZERO             TO  UT1-PRMOMS                               
064600       MOVE ZERO             TO  UT1-PRLEGKST                             
064700       MOVE ZERO             TO  UT1-SUFKTTILL                            
064800       MOVE ZERO             TO  UT1-PRAVDRAG                             
064900       MOVE ZERO             TO  UT1-SUFKTBEL                             
065000       MOVE ZERO             TO  UT1-SUFKTUTL                             
065100       MOVE ZERO             TO  UT1-PRKURS                               
065200       MOVE ZERO             TO  UT1-IDRAPPNR                             
065300       MOVE ZERO             TO  UT1-IDKOLLI                              
065400       MOVE SPACE            TO  UT1-KDANMORS                             
065500       MOVE SPACE            TO  UT1-KDVALISO                             
065600       MOVE ZERO             TO  UT1-DAINLINL                             
065700       MOVE ZERO             TO  UT1-KVANTMOT                             
065800       MOVE ZERO             TO  UT1-KVSKROT                              
065900       MOVE ZERO             TO  UT1-PRAVCOST                             
066000       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
066100       MOVE ZERO             TO  UT1-KVLS-OLD                             
066200       MOVE SPACE            TO  UT1-FLSLUT                               
066300       MOVE ZERO             TO  UT1-REMARKUP                             
066400       MOVE ZERO             TO  UT1-IDKNOTNR                             
066500       MOVE ZERO             TO  UT1-DAKRENOT                             
066600       MOVE ZERO             TO  UT1-SUKREUTL                             
066700       MOVE ZERO             TO  UT1-SUKRENTO                             
066800       MOVE ZERO             TO  UT1-PRLANDCO                             
066900       MOVE ZERO             TO  UT1-SUKRENOT                             
067000       MOVE ZERO             TO  UT1-KVKREANT                             
067100       MOVE ZERO             TO  UT1-DARETILL                             
067200       MOVE ZERO             TO  UT1-KVLEVANM                             
067300       MOVE ZERO             TO  UT1-KVRETINL                             
067400       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
067500       MOVE ZERO             TO  UT1-IDKONTO                              
067600       MOVE SPACE            TO  UT1-IDKST                                
067700       MOVE ZERO             TO  UT1-DAJUSTDA                             
067800       MOVE ZERO             TO  UT1-KVJUSTKV                             
067900       MOVE ZERO             TO  UT1-KDINVKAT                             
068000       MOVE SPACE            TO  UT1-TEINVANM                             
068100       MOVE ZERO             TO  UT1-IDPRODNR                             
068200       MOVE SPACE            TO  UT1-IDLEVNR                              
068300       MOVE ZERO             TO  UT1-KVBEART                              
068400       MOVE ZERO             TO  UT1-DAORDDAT                             
068500       MOVE ZERO             TO  UT1-PRARTBEU                             
068600       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
068700       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
068800       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
068900       MOVE SPACE            TO  UT1-IDFS                                 
069000       MOVE ZERO             TO  UT1-KVAVIS                               
069100       MOVE ZERO             TO  UT1-IDBYTRAP                             
069200       MOVE ZERO             TO  UT1-KVRETUR                              
069300       MOVE ZERO             TO  UT1-SUAVCOST                             
069400       MOVE SPACE            TO  UT1-IDUSER                               
069500       MOVE SPACE            TO  UT1-KDAVCOST                             
069600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
069700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
069800       MOVE ZERO             TO  UT1-KVEFRS                               
069900       MOVE ZERO             TO  UT1-KVLS                                 
070000       MOVE ZERO             TO  UT1-DAREGDAT                             
070100       MOVE ZERO             TO  UT1-DASTADAT                             
070200       MOVE ZERO             TO  UT1-PRKURS-SU                            
070300       MOVE ZERO             TO  UT1-PRKURS-SC                            
070400       MOVE ZERO             TO  UT1-PRKURS-UC                            
070500       MOVE ZERO             TO  UT1-PRKURS-CU                            
070510       PERFORM S11-WRITE-W56022A                                          
070600     END-IF                                                               
070700     IF IN1-IDPTYP = 'A01'                                                
070800       MOVE 1A01-IDPTYP      TO  UT1-IDPTYP                               
070900       MOVE 1A01-KDEKOHT     TO  UT1-KDEKOHT                              
071000       MOVE 1A01-IDFTG       TO  UT1-IDFTG                                
071100       MOVE 1A01-IDDC-SEND   TO  UT1-IDDC-SEND                            
071200       MOVE 1A01-IDDC-REC    TO  UT1-IDDC-REC                             
071300       MOVE 1A01-IDDISTR     TO  UT1-IDDISTR                              
071400       MOVE 1A01-IDKUNDNR    TO  UT1-IDKUNDNR                             
071500       MOVE 1A01-IDFAKT      TO  UT1-IDFAKT                               
071600       MOVE 1A01-KDFAKTYP    TO  UT1-KDFAKTYP                             
071700       MOVE 1A01-DAFAKT      TO  UT1-DAFAKT                               
071800       MOVE ZERO             TO  UT1-IDORDNR7                             
071900       MOVE ZERO             TO  UT1-IDARTNR                              
072000       MOVE ZERO             TO  UT1-KDPRODSL                             
072100       MOVE ZERO             TO  UT1-KDPSLLOC                             
072200       MOVE ZERO             TO  UT1-KVLEVART                             
072300       MOVE ZERO             TO  UT1-PRARTNTO                             
072400       MOVE SPACE            TO  UT1-FLOVRLEV                             
072500       MOVE ZERO             TO  UT1-KDFRAKT                              
072600       MOVE 1A01-SUFAKTRE    TO  UT1-SUFAKTRE                             
072700       MOVE 1A01-PREMBHNT    TO  UT1-PREMBHNT                             
072800       MOVE 1A01-PRFRAKT     TO  UT1-PRFRAKT                              
072900       MOVE 1A01-PRFOERS     TO  UT1-PRFOERS                              
073000       MOVE 1A01-PRMOMS      TO  UT1-PRMOMS                               
073100       MOVE 1A01-PRLEGKST    TO  UT1-PRLEGKST                             
073200       MOVE 1A01-SUFKTTILL   TO  UT1-SUFKTTILL                            
073300       MOVE 1A01-PRAVDRAG    TO  UT1-PRAVDRAG                             
073400       MOVE 1A01-SUFKTBEL    TO  UT1-SUFKTBEL                             
073500       MOVE 1A01-SUFKTUTL    TO  UT1-SUFKTUTL                             
073600       MOVE 1A01-PRKURS      TO  UT1-PRKURS                               
073700       MOVE 1A01-IDRAPPNR    TO  UT1-IDRAPPNR                             
073800       MOVE ZERO             TO  UT1-IDKOLLI                              
073900       MOVE SPACE            TO  UT1-KDANMORS                             
074000       MOVE SPACE            TO  UT1-KDVALISO                             
074100       MOVE ZERO             TO  UT1-DAINLINL                             
074200       MOVE ZERO             TO  UT1-KVANTMOT                             
074300       MOVE ZERO             TO  UT1-KVSKROT                              
074400       MOVE ZERO             TO  UT1-PRAVCOST                             
074500       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
074600       MOVE ZERO             TO  UT1-KVLS-OLD                             
074700       MOVE SPACE            TO  UT1-FLSLUT                               
074800       MOVE ZERO             TO  UT1-REMARKUP                             
074900       MOVE ZERO             TO  UT1-IDKNOTNR                             
075000       MOVE ZERO             TO  UT1-DAKRENOT                             
075100       MOVE ZERO             TO  UT1-SUKREUTL                             
075200       MOVE ZERO             TO  UT1-SUKRENTO                             
075300       MOVE ZERO             TO  UT1-PRLANDCO                             
075400       MOVE ZERO             TO  UT1-SUKRENOT                             
075500       MOVE ZERO             TO  UT1-KVKREANT                             
075600       MOVE ZERO             TO  UT1-DARETILL                             
075700       MOVE ZERO             TO  UT1-KVLEVANM                             
075800       MOVE ZERO             TO  UT1-KVRETINL                             
075900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
076000       MOVE ZERO             TO  UT1-IDKONTO                              
076100       MOVE SPACE            TO  UT1-IDKST                                
076200       MOVE ZERO             TO  UT1-DAJUSTDA                             
076300       MOVE ZERO             TO  UT1-KVJUSTKV                             
076400       MOVE ZERO             TO  UT1-KDINVKAT                             
076500       MOVE SPACE            TO  UT1-TEINVANM                             
076600       MOVE ZERO             TO  UT1-IDPRODNR                             
076700       MOVE SPACE            TO  UT1-IDLEVNR                              
076800       MOVE ZERO             TO  UT1-KVBEART                              
076900       MOVE ZERO             TO  UT1-DAORDDAT                             
077000       MOVE ZERO             TO  UT1-PRARTBEU                             
077100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
077200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
077300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
077400       MOVE SPACE            TO  UT1-IDFS                                 
077500       MOVE ZERO             TO  UT1-KVAVIS                               
077600       MOVE ZERO             TO  UT1-IDBYTRAP                             
077700       MOVE ZERO             TO  UT1-KVRETUR                              
077800       MOVE ZERO             TO  UT1-SUAVCOST                             
077900       MOVE SPACE            TO  UT1-IDUSER                               
078000       MOVE SPACE            TO  UT1-KDAVCOST                             
078100       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
078200       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
078300       MOVE ZERO             TO  UT1-KVEFRS                               
078400       MOVE ZERO             TO  UT1-KVLS                                 
078500       MOVE ZERO             TO  UT1-DAREGDAT                             
078600       MOVE ZERO             TO  UT1-DASTADAT                             
078700       MOVE ZERO             TO  UT1-PRKURS-SU                            
078800       MOVE ZERO             TO  UT1-PRKURS-SC                            
078900       MOVE ZERO             TO  UT1-PRKURS-UC                            
079000       MOVE ZERO             TO  UT1-PRKURS-CU                            
079010       PERFORM S11-WRITE-W56022A                                          
079100     END-IF                                                               
079200     IF IN1-IDPTYP = 'A02'                                                
079300       MOVE 1A02-IDPTYP      TO  UT1-IDPTYP                               
079400       MOVE 1A02-KDEKOHT     TO  UT1-KDEKOHT                              
079500       MOVE 1A02-IDFTG       TO  UT1-IDFTG                                
079600       MOVE 1A02-IDDC-SEND   TO  UT1-IDDC-SEND                            
079700       MOVE 1A02-IDDC-REC    TO  UT1-IDDC-REC                             
079800       MOVE 1A02-IDDISTR     TO  UT1-IDDISTR                              
079900       MOVE 1A02-IDKUNDNR    TO  UT1-IDKUNDNR                             
080000       MOVE 1A02-IDFAKT      TO  UT1-IDFAKT                               
080100       MOVE 1A02-KDFAKTYP    TO  UT1-KDFAKTYP                             
080200       MOVE 1A02-DAFAKT      TO  UT1-DAFAKT                               
080300       MOVE 1A02-IDORDNR7    TO  UT1-IDORDNR7                             
080400       MOVE 1A02-IDARTNR     TO  UT1-IDARTNR                              
080500       MOVE 1A02-KDPRODSL    TO  UT1-KDPRODSL                             
080600       MOVE 1A02-KDPSLLOC    TO  UT1-KDPSLLOC                             
080700       MOVE 1A02-KVLEVART    TO  UT1-KVLEVART                             
080800       MOVE 1A02-PRARTNTO    TO  UT1-PRARTNTO                             
080900       MOVE SPACE            TO  UT1-FLOVRLEV                             
081000       MOVE ZERO             TO  UT1-KDFRAKT                              
081100       MOVE ZERO             TO  UT1-SUFAKTRE                             
081200       MOVE ZERO             TO  UT1-PREMBHNT                             
081300       MOVE ZERO             TO  UT1-PRFRAKT                              
081400       MOVE ZERO             TO  UT1-PRFOERS                              
081500       MOVE ZERO             TO  UT1-PRMOMS                               
081600       MOVE ZERO             TO  UT1-PRLEGKST                             
081700       MOVE ZERO             TO  UT1-SUFKTTILL                            
081800       MOVE ZERO             TO  UT1-PRAVDRAG                             
081900       MOVE ZERO             TO  UT1-SUFKTBEL                             
082000       MOVE ZERO             TO  UT1-SUFKTUTL                             
082100       MOVE ZERO             TO  UT1-PRKURS                               
082200       MOVE ZERO             TO  UT1-IDRAPPNR                             
082300       MOVE ZERO             TO  UT1-IDKOLLI                              
082400       MOVE SPACE            TO  UT1-KDANMORS                             
082500       MOVE SPACE            TO  UT1-KDVALISO                             
082600       MOVE ZERO             TO  UT1-DAINLINL                             
082700       MOVE ZERO             TO  UT1-KVANTMOT                             
082800       MOVE ZERO             TO  UT1-KVSKROT                              
082900       MOVE ZERO             TO  UT1-PRAVCOST                             
083000       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
083100       MOVE ZERO             TO  UT1-KVLS-OLD                             
083200       MOVE SPACE            TO  UT1-FLSLUT                               
083300       MOVE ZERO             TO  UT1-REMARKUP                             
083400       MOVE ZERO             TO  UT1-IDKNOTNR                             
083500       MOVE ZERO             TO  UT1-DAKRENOT                             
083600       MOVE ZERO             TO  UT1-SUKREUTL                             
083700       MOVE ZERO             TO  UT1-SUKRENTO                             
083800       MOVE ZERO             TO  UT1-PRLANDCO                             
083900       MOVE ZERO             TO  UT1-SUKRENOT                             
084000       MOVE ZERO             TO  UT1-KVKREANT                             
084100       MOVE ZERO             TO  UT1-DARETILL                             
084200       MOVE ZERO             TO  UT1-KVLEVANM                             
084300       MOVE ZERO             TO  UT1-KVRETINL                             
084400       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
084500       MOVE ZERO             TO  UT1-IDKONTO                              
084600       MOVE SPACE            TO  UT1-IDKST                                
084700       MOVE ZERO             TO  UT1-DAJUSTDA                             
084800       MOVE ZERO             TO  UT1-KVJUSTKV                             
084900       MOVE ZERO             TO  UT1-KDINVKAT                             
085000       MOVE SPACE            TO  UT1-TEINVANM                             
085100       MOVE ZERO             TO  UT1-IDPRODNR                             
085200       MOVE SPACE            TO  UT1-IDLEVNR                              
085300       MOVE ZERO             TO  UT1-KVBEART                              
085400       MOVE ZERO             TO  UT1-DAORDDAT                             
085500       MOVE ZERO             TO  UT1-PRARTBEU                             
085600       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
085700       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
085800       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
085900       MOVE SPACE            TO  UT1-IDFS                                 
086000       MOVE ZERO             TO  UT1-KVAVIS                               
086100       MOVE ZERO             TO  UT1-IDBYTRAP                             
086200       MOVE ZERO             TO  UT1-KVRETUR                              
086300       MOVE ZERO             TO  UT1-SUAVCOST                             
086400       MOVE SPACE            TO  UT1-IDUSER                               
086500       MOVE SPACE            TO  UT1-KDAVCOST                             
086600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
086700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
086800       MOVE ZERO             TO  UT1-KVEFRS                               
086900       MOVE ZERO             TO  UT1-KVLS                                 
087000       MOVE ZERO             TO  UT1-DAREGDAT                             
087100       MOVE ZERO             TO  UT1-DASTADAT                             
087200       MOVE ZERO             TO  UT1-PRKURS-SU                            
087300       MOVE ZERO             TO  UT1-PRKURS-SC                            
087400       MOVE ZERO             TO  UT1-PRKURS-UC                            
087500       MOVE ZERO             TO  UT1-PRKURS-CU                            
087510       PERFORM S11-WRITE-W56022A                                          
087600     END-IF                                                               
087700     IF IN1-IDPTYP = 'A03'                                                
087800       MOVE 1A03-IDPTYP      TO  UT1-IDPTYP                               
087900       MOVE 1A03-KDEKOHT     TO  UT1-KDEKOHT                              
088000       MOVE 1A03-IDFTG       TO  UT1-IDFTG                                
088100       MOVE 1A03-IDDC-SEND   TO  UT1-IDDC-SEND                            
088200       MOVE 1A03-IDDC-REC    TO  UT1-IDDC-REC                             
088300       MOVE 1A03-IDDISTR     TO  UT1-IDDISTR                              
088400       MOVE 1A03-IDKUNDNR    TO  UT1-IDKUNDNR                             
088500       MOVE 1A03-IDFAKT      TO  UT1-IDFAKT                               
088600       MOVE space            TO  UT1-KDFAKTYP                             
088700       MOVE 1A03-DAFAKT      TO  UT1-DAFAKT                               
088800       MOVE 1A03-IDORDNR7    TO  UT1-IDORDNR7                             
088900       MOVE 1A03-IDARTNR     TO  UT1-IDARTNR                              
089000       MOVE 1A03-KDPRODSL    TO  UT1-KDPRODSL                             
089100       MOVE 1A03-KDPSLLOC    TO  UT1-KDPSLLOC                             
089200       MOVE 1A03-KVLEVART    TO  UT1-KVLEVART                             
089300       MOVE 1A03-PRARTNTO    TO  UT1-PRARTNTO                             
089400       MOVE SPACE            TO  UT1-FLOVRLEV                             
089500       MOVE ZERO             TO  UT1-KDFRAKT                              
089600       MOVE ZERO             TO  UT1-SUFAKTRE                             
089700       MOVE ZERO             TO  UT1-PREMBHNT                             
089800       MOVE ZERO             TO  UT1-PRFRAKT                              
089900       MOVE ZERO             TO  UT1-PRFOERS                              
090000       MOVE ZERO             TO  UT1-PRMOMS                               
090100       MOVE ZERO             TO  UT1-PRLEGKST                             
090200       MOVE ZERO             TO  UT1-SUFKTTILL                            
090300       MOVE ZERO             TO  UT1-PRAVDRAG                             
090400       MOVE ZERO             TO  UT1-SUFKTBEL                             
090500       MOVE ZERO             TO  UT1-SUFKTUTL                             
090600       MOVE ZERO             TO  UT1-PRKURS                               
090700       MOVE ZERO             TO  UT1-IDRAPPNR                             
090800       MOVE 1A03-IDKOLLI     TO  UT1-IDKOLLI                              
090900       MOVE 1A03-KDANMORS    TO  UT1-KDANMORS                             
091000       MOVE 1A03-KDVALISO    TO  UT1-KDVALISO                             
091100       MOVE 1A03-DAINLINL    TO  UT1-DAINLINL                             
091200       MOVE 1A03-KVANTMOT    TO  UT1-KVANTMOT                             
091300       MOVE 1A03-KVSKROT     TO  UT1-KVSKROT                              
091400       MOVE 1A03-PRAVCOST    TO  UT1-PRAVCOST                             
091500       MOVE 1A03-PRAVCOST-OLD TO UT1-PRAVCOST-OLD                         
091600       MOVE ZERO             TO  UT1-KVLS-OLD                             
091700       MOVE SPACE            TO  UT1-FLSLUT                               
091800       MOVE ZERO             TO  UT1-REMARKUP                             
091900       MOVE ZERO             TO  UT1-IDKNOTNR                             
092000       MOVE ZERO             TO  UT1-DAKRENOT                             
092100       MOVE ZERO             TO  UT1-SUKREUTL                             
092200       MOVE ZERO             TO  UT1-SUKRENTO                             
092300       MOVE ZERO             TO  UT1-PRLANDCO                             
092400       MOVE ZERO             TO  UT1-SUKRENOT                             
092500       MOVE ZERO             TO  UT1-KVKREANT                             
092600       MOVE ZERO             TO  UT1-DARETILL                             
092700       MOVE ZERO             TO  UT1-KVLEVANM                             
092800       MOVE ZERO             TO  UT1-KVRETINL                             
092900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
093000       MOVE ZERO             TO  UT1-IDKONTO                              
093100       MOVE SPACE            TO  UT1-IDKST                                
093200       MOVE ZERO             TO  UT1-DAJUSTDA                             
093300       MOVE ZERO             TO  UT1-KVJUSTKV                             
093400       MOVE ZERO             TO  UT1-KDINVKAT                             
093500       MOVE SPACE            TO  UT1-TEINVANM                             
093600       MOVE ZERO             TO  UT1-IDPRODNR                             
093700       MOVE SPACE            TO  UT1-IDLEVNR                              
093800       MOVE ZERO             TO  UT1-KVBEART                              
093900       MOVE ZERO             TO  UT1-DAORDDAT                             
094000       MOVE ZERO             TO  UT1-PRARTBEU                             
094100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
094200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
094300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
094400       MOVE SPACE            TO  UT1-IDFS                                 
094500       MOVE ZERO             TO  UT1-KVAVIS                               
094600       MOVE ZERO             TO  UT1-IDBYTRAP                             
094700       MOVE ZERO             TO  UT1-KVRETUR                              
094800       MOVE ZERO             TO  UT1-SUAVCOST                             
094900       MOVE SPACE            TO  UT1-IDUSER                               
095000       MOVE SPACE            TO  UT1-KDAVCOST                             
095100       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
095200       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
095300       MOVE ZERO             TO  UT1-KVEFRS                               
095400       MOVE ZERO             TO  UT1-KVLS                                 
095500       MOVE ZERO             TO  UT1-DAREGDAT                             
095600       MOVE ZERO             TO  UT1-DASTADAT                             
095700       MOVE ZERO             TO  UT1-PRKURS-SU                            
095800       MOVE ZERO             TO  UT1-PRKURS-SC                            
095900       MOVE ZERO             TO  UT1-PRKURS-UC                            
096000       MOVE ZERO             TO  UT1-PRKURS-CU                            
096010       PERFORM S11-WRITE-W56022A                                          
096100     END-IF                                                               
096200     IF IN1-IDPTYP = 'A04'                                                
096300       MOVE 1A04-IDPTYP      TO  UT1-IDPTYP                               
096400       MOVE 1A04-KDEKOHT     TO  UT1-KDEKOHT                              
096500       MOVE 1A04-IDFTG       TO  UT1-IDFTG                                
096600       MOVE 1A04-IDDC-SEND   TO  UT1-IDDC-SEND                            
096700       MOVE 1A04-IDDC-REC    TO  UT1-IDDC-REC                             
096800       MOVE 1A04-IDDISTR     TO  UT1-IDDISTR                              
096900       MOVE 1A04-IDKUNDNR    TO  UT1-IDKUNDNR                             
097000       MOVE ZERO             TO  UT1-IDFAKT                               
097100       MOVE SPACE            TO  UT1-KDFAKTYP                             
097200       MOVE ZERO             TO  UT1-DAFAKT                               
097300       MOVE ZERO             TO  UT1-IDORDNR7                             
097400       MOVE ZERO             TO  UT1-IDARTNR                              
097500       MOVE ZERO             TO  UT1-KDPRODSL                             
097600       MOVE ZERO             TO  UT1-KDPSLLOC                             
097700       MOVE ZERO             TO  UT1-KVLEVART                             
097800       MOVE ZERO             TO  UT1-PRARTNTO                             
097900       MOVE SPACE            TO  UT1-FLOVRLEV                             
098000       MOVE ZERO             TO  UT1-KDFRAKT                              
098100       MOVE ZERO             TO  UT1-SUFAKTRE                             
098200       MOVE 1A04-PREMBHNT    TO  UT1-PREMBHNT                             
098300       MOVE 1A04-PRFRAKT     TO  UT1-PRFRAKT                              
098400       MOVE 1A04-PRFOERS     TO  UT1-PRFOERS                              
098500       MOVE 1A04-PRMOMS      TO  UT1-PRMOMS                               
098600       MOVE 1A04-PRLEGKST    TO  UT1-PRLEGKST                             
098700       MOVE ZERO             TO  UT1-SUFKTTILL                            
098800       MOVE ZERO             TO  UT1-PRAVDRAG                             
098900       MOVE ZERO             TO  UT1-SUFKTBEL                             
099000       MOVE ZERO             TO  UT1-SUFKTUTL                             
099100       MOVE 1A04-PRKURS      TO  UT1-PRKURS                               
099200       MOVE 1A04-IDRAPPNR    TO  UT1-IDRAPPNR                             
099300       MOVE ZERO             TO  UT1-IDKOLLI                              
099400       MOVE SPACE            TO  UT1-KDANMORS                             
099500       MOVE SPACE            TO  UT1-KDVALISO                             
099600       MOVE ZERO             TO  UT1-DAINLINL                             
099700       MOVE ZERO             TO  UT1-KVANTMOT                             
099800       MOVE ZERO             TO  UT1-KVSKROT                              
099900       MOVE ZERO             TO  UT1-PRAVCOST                             
100000       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
100100       MOVE ZERO             TO  UT1-KVLS-OLD                             
100200       MOVE SPACE            TO  UT1-FLSLUT                               
100300       MOVE ZERO             TO  UT1-REMARKUP                             
100400       MOVE 1A04-IDKNOTNR    TO  UT1-IDKNOTNR                             
100500       MOVE 1A04-DAKRENOT    TO  UT1-DAKRENOT                             
100600       MOVE 1A04-SUKREUTL    TO  UT1-SUKREUTL                             
100700       MOVE 1A04-SUKRENTO    TO  UT1-SUKRENTO                             
100800       MOVE 1A04-PRLANDCO    TO  UT1-PRLANDCO                             
100900       MOVE 1A04-SUKRENOT    TO  UT1-SUKRENOT                             
101000       MOVE ZERO             TO  UT1-KVKREANT                             
101100       MOVE ZERO             TO  UT1-DARETILL                             
101200       MOVE ZERO             TO  UT1-KVLEVANM                             
101300       MOVE ZERO             TO  UT1-KVRETINL                             
101400       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
101500       MOVE ZERO             TO  UT1-IDKONTO                              
101600       MOVE SPACE            TO  UT1-IDKST                                
101700       MOVE ZERO             TO  UT1-DAJUSTDA                             
101800       MOVE ZERO             TO  UT1-KVJUSTKV                             
101900       MOVE ZERO             TO  UT1-KDINVKAT                             
102000       MOVE SPACE            TO  UT1-TEINVANM                             
102100       MOVE ZERO             TO  UT1-IDPRODNR                             
102200       MOVE SPACE            TO  UT1-IDLEVNR                              
102300       MOVE ZERO             TO  UT1-KVBEART                              
102400       MOVE ZERO             TO  UT1-DAORDDAT                             
102500       MOVE ZERO             TO  UT1-PRARTBEU                             
102600       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
102700       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
102800       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
102900       MOVE SPACE            TO  UT1-IDFS                                 
103000       MOVE ZERO             TO  UT1-KVAVIS                               
103100       MOVE ZERO             TO  UT1-IDBYTRAP                             
103200       MOVE ZERO             TO  UT1-KVRETUR                              
103300       MOVE ZERO             TO  UT1-SUAVCOST                             
103400       MOVE SPACE            TO  UT1-IDUSER                               
103500       MOVE SPACE            TO  UT1-KDAVCOST                             
103600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
103700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
103800       MOVE ZERO             TO  UT1-KVEFRS                               
103900       MOVE ZERO             TO  UT1-KVLS                                 
104000       MOVE ZERO             TO  UT1-DAREGDAT                             
104100       MOVE ZERO             TO  UT1-DASTADAT                             
104200       MOVE ZERO             TO  UT1-PRKURS-SU                            
104300       MOVE ZERO             TO  UT1-PRKURS-SC                            
104400       MOVE ZERO             TO  UT1-PRKURS-UC                            
104500       MOVE ZERO             TO  UT1-PRKURS-CU                            
104510       PERFORM S11-WRITE-W56022A                                          
104600     END-IF                                                               
104700     IF IN1-IDPTYP = 'A05'                                                
104800       MOVE 1A05-IDPTYP      TO  UT1-IDPTYP                               
104900       MOVE 1A05-KDEKOHT     TO  UT1-KDEKOHT                              
105000       MOVE 1A05-IDFTG       TO  UT1-IDFTG                                
105100       MOVE 1A05-IDDC-SEND   TO  UT1-IDDC-SEND                            
105200       MOVE 1A05-IDDC-REC    TO  UT1-IDDC-REC                             
105300       MOVE 1A05-IDDISTR     TO  UT1-IDDISTR                              
105400       MOVE 1A05-IDKUNDNR    TO  UT1-IDKUNDNR                             
105500       MOVE ZERO             TO  UT1-IDFAKT                               
105600       MOVE SPACE            TO  UT1-KDFAKTYP                             
105700       MOVE ZERO             TO  UT1-DAFAKT                               
105800       MOVE ZERO             TO  UT1-IDORDNR7                             
105900       MOVE 1A05-IDARTNR     TO  UT1-IDARTNR                              
106000       MOVE 1A05-KDPRODSL    TO  UT1-KDPRODSL                             
106100       MOVE 1A05-KDPSLLOC    TO  UT1-KDPSLLOC                             
106200       MOVE ZERO             TO  UT1-KVLEVART                             
106300       MOVE 1A05-PRARTNTO    TO  UT1-PRARTNTO                             
106400       MOVE SPACE            TO  UT1-FLOVRLEV                             
106500       MOVE ZERO             TO  UT1-KDFRAKT                              
106600       MOVE ZERO             TO  UT1-SUFAKTRE                             
106700       MOVE ZERO             TO  UT1-PREMBHNT                             
106800       MOVE ZERO             TO  UT1-PRFRAKT                              
106900       MOVE ZERO             TO  UT1-PRFOERS                              
107000       MOVE ZERO             TO  UT1-PRMOMS                               
107100       MOVE ZERO             TO  UT1-PRLEGKST                             
107200       MOVE ZERO             TO  UT1-SUFKTTILL                            
107300       MOVE ZERO             TO  UT1-PRAVDRAG                             
107400       MOVE ZERO             TO  UT1-SUFKTBEL                             
107500       MOVE ZERO             TO  UT1-SUFKTUTL                             
107600       MOVE ZERO             TO  UT1-PRKURS                               
107700       MOVE ZERO             TO  UT1-IDRAPPNR                             
107800       MOVE ZERO             TO  UT1-IDKOLLI                              
107900       MOVE 1A05-KDANMORS    TO  UT1-KDANMORS                             
108000       MOVE SPACE            TO  UT1-KDVALISO                             
108100       MOVE ZERO             TO  UT1-DAINLINL                             
108200       MOVE ZERO             TO  UT1-KVANTMOT                             
108300       MOVE ZERO             TO  UT1-KVSKROT                              
108400       MOVE ZERO             TO  UT1-PRAVCOST                             
108500       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
108600       MOVE ZERO             TO  UT1-KVLS-OLD                             
108700       MOVE SPACE            TO  UT1-FLSLUT                               
108800       MOVE ZERO             TO  UT1-REMARKUP                             
108900       MOVE 1A05-IDKNOTNR    TO  UT1-IDKNOTNR                             
109000       MOVE 1A05-DAKRENOT    TO  UT1-DAKRENOT                             
109100       MOVE ZERO             TO  UT1-SUKREUTL                             
109200       MOVE ZERO             TO  UT1-SUKRENTO                             
109300       MOVE ZERO             TO  UT1-PRLANDCO                             
109400       MOVE ZERO             TO  UT1-SUKRENOT                             
109500       MOVE 1A05-KVKREANT    TO  UT1-KVKREANT                             
109600       MOVE ZERO             TO  UT1-DARETILL                             
109700       MOVE ZERO             TO  UT1-KVLEVANM                             
109800       MOVE ZERO             TO  UT1-KVRETINL                             
109900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
110000       MOVE ZERO             TO  UT1-IDKONTO                              
110100       MOVE SPACE            TO  UT1-IDKST                                
110200       MOVE ZERO             TO  UT1-DAJUSTDA                             
110300       MOVE ZERO             TO  UT1-KVJUSTKV                             
110400       MOVE ZERO             TO  UT1-KDINVKAT                             
110500       MOVE SPACE            TO  UT1-TEINVANM                             
110600       MOVE ZERO             TO  UT1-IDPRODNR                             
110700       MOVE SPACE            TO  UT1-IDLEVNR                              
110800       MOVE ZERO             TO  UT1-KVBEART                              
110900       MOVE ZERO             TO  UT1-DAORDDAT                             
111000       MOVE ZERO             TO  UT1-PRARTBEU                             
111100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
111200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
111300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
111400       MOVE SPACE            TO  UT1-IDFS                                 
111500       MOVE ZERO             TO  UT1-KVAVIS                               
111600       MOVE ZERO             TO  UT1-IDBYTRAP                             
111700       MOVE ZERO             TO  UT1-KVRETUR                              
111800       MOVE ZERO             TO  UT1-SUAVCOST                             
111900       MOVE SPACE            TO  UT1-IDUSER                               
112000       MOVE SPACE            TO  UT1-KDAVCOST                             
112100       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
112200       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
112300       MOVE ZERO             TO  UT1-KVEFRS                               
112400       MOVE ZERO             TO  UT1-KVLS                                 
112500       MOVE ZERO             TO  UT1-DAREGDAT                             
112600       MOVE ZERO             TO  UT1-DASTADAT                             
112700       MOVE ZERO             TO  UT1-PRKURS-SU                            
112800       MOVE ZERO             TO  UT1-PRKURS-SC                            
112900       MOVE ZERO             TO  UT1-PRKURS-UC                            
113000       MOVE ZERO             TO  UT1-PRKURS-CU                            
113010       PERFORM S11-WRITE-W56022A                                          
113100     END-IF                                                               
113200     IF IN1-IDPTYP = 'A06'                                                
113300       MOVE 1A06-IDPTYP      TO  UT1-IDPTYP                               
113400       MOVE 1A06-KDEKOHT     TO  UT1-KDEKOHT                              
113500       MOVE 1A06-IDFTG       TO  UT1-IDFTG                                
113600       MOVE 1A06-IDDC-SEND   TO  UT1-IDDC-SEND                            
113700       MOVE 1A06-IDDC-REC    TO  UT1-IDDC-REC                             
113800       MOVE 1A06-IDDISTR     TO  UT1-IDDISTR                              
113900       MOVE 1A06-IDKUNDNR    TO  UT1-IDKUNDNR                             
114000       MOVE ZERO             TO  UT1-IDFAKT                               
114100       MOVE SPACE            TO  UT1-KDFAKTYP                             
114200       MOVE ZERO             TO  UT1-DAFAKT                               
114300       MOVE ZERO             TO  UT1-IDORDNR7                             
114400       MOVE 1A06-IDARTNR     TO  UT1-IDARTNR                              
114500       MOVE 1A06-KDPRODSL    TO  UT1-KDPRODSL                             
114600       MOVE 1A06-KDPSLLOC    TO  UT1-KDPSLLOC                             
114700       MOVE ZERO             TO  UT1-KVLEVART                             
114800       MOVE ZERO             TO  UT1-PRARTNTO                             
114900       MOVE SPACE            TO  UT1-FLOVRLEV                             
115000       MOVE ZERO             TO  UT1-KDFRAKT                              
115100       MOVE ZERO             TO  UT1-SUFAKTRE                             
115200       MOVE ZERO             TO  UT1-PREMBHNT                             
115300       MOVE ZERO             TO  UT1-PRFRAKT                              
115400       MOVE ZERO             TO  UT1-PRFOERS                              
115500       MOVE ZERO             TO  UT1-PRMOMS                               
115600       MOVE ZERO             TO  UT1-PRLEGKST                             
115700       MOVE ZERO             TO  UT1-SUFKTTILL                            
115800       MOVE ZERO             TO  UT1-PRAVDRAG                             
115900       MOVE ZERO             TO  UT1-SUFKTBEL                             
116000       MOVE ZERO             TO  UT1-SUFKTUTL                             
116100       MOVE ZERO             TO  UT1-PRKURS                               
116200       MOVE 1A06-IDRAPPNR    TO  UT1-IDRAPPNR                             
116300       MOVE ZERO             TO  UT1-IDKOLLI                              
116400       MOVE 1A06-KDANMORS    TO  UT1-KDANMORS                             
116500       MOVE SPACE            TO  UT1-KDVALISO                             
116600       MOVE ZERO             TO  UT1-DAINLINL                             
116700       MOVE ZERO             TO  UT1-KVANTMOT                             
116800       MOVE ZERO             TO  UT1-KVSKROT                              
116900       MOVE 1A06-PRAVCOST    TO  UT1-PRAVCOST                             
117000       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
117100       MOVE ZERO             TO  UT1-KVLS-OLD                             
117200       MOVE SPACE            TO  UT1-FLSLUT                               
117300       MOVE ZERO             TO  UT1-REMARKUP                             
117400       MOVE ZERO             TO  UT1-IDKNOTNR                             
117500       MOVE ZERO             TO  UT1-DAKRENOT                             
117600       MOVE ZERO             TO  UT1-SUKREUTL                             
117700       MOVE ZERO             TO  UT1-SUKRENTO                             
117800       MOVE ZERO             TO  UT1-PRLANDCO                             
117900       MOVE ZERO             TO  UT1-SUKRENOT                             
118000       MOVE ZERO             TO  UT1-KVKREANT                             
118100       MOVE 1A06-DARETILL    TO  UT1-DARETILL                             
118200       MOVE 1A06-KVLEVANM    TO  UT1-KVLEVANM                             
118300       MOVE 1A06-KVRETINL    TO  UT1-KVRETINL                             
118400       MOVE 1A06-KVRETINL-SKR TO UT1-KVRETINL-SKR                         
118500       MOVE ZERO             TO  UT1-IDKONTO                              
118600       MOVE SPACE            TO  UT1-IDKST                                
118700       MOVE ZERO             TO  UT1-DAJUSTDA                             
118800       MOVE ZERO             TO  UT1-KVJUSTKV                             
118900       MOVE ZERO             TO  UT1-KDINVKAT                             
119000       MOVE SPACE            TO  UT1-TEINVANM                             
119100       MOVE ZERO             TO  UT1-IDPRODNR                             
119200       MOVE SPACE            TO  UT1-IDLEVNR                              
119300       MOVE ZERO             TO  UT1-KVBEART                              
119400       MOVE ZERO             TO  UT1-DAORDDAT                             
119500       MOVE ZERO             TO  UT1-PRARTBEU                             
119600       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
119700       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
119800       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
119900       MOVE SPACE            TO  UT1-IDFS                                 
120000       MOVE ZERO             TO  UT1-KVAVIS                               
120100       MOVE ZERO             TO  UT1-IDBYTRAP                             
120200       MOVE ZERO             TO  UT1-KVRETUR                              
120300       MOVE ZERO             TO  UT1-SUAVCOST                             
120400       MOVE SPACE            TO  UT1-IDUSER                               
120500       MOVE SPACE            TO  UT1-KDAVCOST                             
120600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
120700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
120800       MOVE ZERO             TO  UT1-KVEFRS                               
120900       MOVE ZERO             TO  UT1-KVLS                                 
121000       MOVE ZERO             TO  UT1-DAREGDAT                             
121100       MOVE ZERO             TO  UT1-DASTADAT                             
121200       MOVE ZERO             TO  UT1-PRKURS-SU                            
121300       MOVE ZERO             TO  UT1-PRKURS-SC                            
121400       MOVE ZERO             TO  UT1-PRKURS-UC                            
121500       MOVE ZERO             TO  UT1-PRKURS-CU                            
121510       PERFORM S11-WRITE-W56022A                                          
121600     END-IF                                                               
121700     IF IN1-IDPTYP = 'A07'                                                
121800       MOVE 1A07-IDPTYP      TO  UT1-IDPTYP                               
121900       MOVE 1A07-KDEKOHT     TO  UT1-KDEKOHT                              
122000       MOVE 1A07-IDFTG       TO  UT1-IDFTG                                
122100       MOVE 1A07-IDDC-SEND   TO  UT1-IDDC-SEND                            
122200       MOVE 1A07-IDDC-REC    TO  UT1-IDDC-REC                             
122300       MOVE 1A07-IDDISTR     TO  UT1-IDDISTR                              
122400       MOVE 1A07-IDKUNDNR    TO  UT1-IDKUNDNR                             
122500       MOVE 1A07-IDFAKT      TO  UT1-IDFAKT                               
122600       MOVE SPACE            TO  UT1-KDFAKTYP                             
122700       MOVE 1A07-DAFAKT      TO  UT1-DAFAKT                               
122800       MOVE 1A07-IDORDNR7    TO  UT1-IDORDNR7                             
122900       MOVE 1A07-IDARTNR     TO  UT1-IDARTNR                              
123000       MOVE 1A07-KDPRODSL    TO  UT1-KDPRODSL                             
123100       MOVE 1A07-KDPSLLOC    TO  UT1-KDPSLLOC                             
123200       MOVE 1A07-KVLEVART    TO  UT1-KVLEVART                             
123300       MOVE ZERO             TO  UT1-PRARTNTO                             
123400       MOVE SPACE            TO  UT1-FLOVRLEV                             
123500       MOVE ZERO             TO  UT1-KDFRAKT                              
123600       MOVE ZERO             TO  UT1-SUFAKTRE                             
123700       MOVE ZERO             TO  UT1-PREMBHNT                             
123800       MOVE ZERO             TO  UT1-PRFRAKT                              
123900       MOVE ZERO             TO  UT1-PRFOERS                              
124000       MOVE ZERO             TO  UT1-PRMOMS                               
124100       MOVE ZERO             TO  UT1-PRLEGKST                             
124200       MOVE ZERO             TO  UT1-SUFKTTILL                            
124300       MOVE ZERO             TO  UT1-PRAVDRAG                             
124400       MOVE ZERO             TO  UT1-SUFKTBEL                             
124500       MOVE ZERO             TO  UT1-SUFKTUTL                             
124600       MOVE ZERO             TO  UT1-PRKURS                               
124700       MOVE ZERO             TO  UT1-IDRAPPNR                             
124800       MOVE ZERO             TO  UT1-IDKOLLI                              
124900       MOVE SPACE            TO  UT1-KDANMORS                             
125000       MOVE SPACE            TO  UT1-KDVALISO                             
125100       MOVE ZERO             TO  UT1-DAINLINL                             
125200       MOVE ZERO             TO  UT1-KVANTMOT                             
125300       MOVE ZERO             TO  UT1-KVSKROT                              
125400       MOVE 1A07-PRAVCOST    TO  UT1-PRAVCOST                             
125500       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
125600       MOVE ZERO             TO  UT1-KVLS-OLD                             
125700       MOVE SPACE            TO  UT1-FLSLUT                               
125800       MOVE ZERO             TO  UT1-REMARKUP                             
125900       MOVE ZERO             TO  UT1-IDKNOTNR                             
126000       MOVE ZERO             TO  UT1-DAKRENOT                             
126100       MOVE ZERO             TO  UT1-SUKREUTL                             
126200       MOVE ZERO             TO  UT1-SUKRENTO                             
126300       MOVE ZERO             TO  UT1-PRLANDCO                             
126400       MOVE ZERO             TO  UT1-SUKRENOT                             
126500       MOVE ZERO             TO  UT1-KVKREANT                             
126600       MOVE ZERO             TO  UT1-DARETILL                             
126700       MOVE ZERO             TO  UT1-KVLEVANM                             
126800       MOVE ZERO             TO  UT1-KVRETINL                             
126900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
127000       MOVE 1A07-IDKONTO     TO  UT1-IDKONTO                              
127100       MOVE 1A07-IDKST       TO  UT1-IDKST                                
127200       MOVE ZERO             TO  UT1-DAJUSTDA                             
127300       MOVE ZERO             TO  UT1-KVJUSTKV                             
127400       MOVE ZERO             TO  UT1-KDINVKAT                             
127500       MOVE SPACE            TO  UT1-TEINVANM                             
127600       MOVE ZERO             TO  UT1-IDPRODNR                             
127700       MOVE SPACE            TO  UT1-IDLEVNR                              
127800       MOVE ZERO             TO  UT1-KVBEART                              
127900       MOVE ZERO             TO  UT1-DAORDDAT                             
128000       MOVE ZERO             TO  UT1-PRARTBEU                             
128100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
128200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
128300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
128400       MOVE SPACE            TO  UT1-IDFS                                 
128500       MOVE ZERO             TO  UT1-KVAVIS                               
128600       MOVE ZERO             TO  UT1-IDBYTRAP                             
128700       MOVE ZERO             TO  UT1-KVRETUR                              
128800       MOVE ZERO             TO  UT1-SUAVCOST                             
128900       MOVE SPACE            TO  UT1-IDUSER                               
129000       MOVE SPACE            TO  UT1-KDAVCOST                             
129100       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
129200       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
129300       MOVE ZERO             TO  UT1-KVEFRS                               
129400       MOVE ZERO             TO  UT1-KVLS                                 
129500       MOVE ZERO             TO  UT1-DAREGDAT                             
129600       MOVE ZERO             TO  UT1-DASTADAT                             
129700       MOVE ZERO             TO  UT1-PRKURS-SU                            
129800       MOVE ZERO             TO  UT1-PRKURS-SC                            
129900       MOVE ZERO             TO  UT1-PRKURS-UC                            
130000       MOVE ZERO             TO  UT1-PRKURS-CU                            
130010       PERFORM S11-WRITE-W56022A                                          
130100     END-IF                                                               
130200     IF IN1-IDPTYP = 'A08'                                                
130300       MOVE 1A08-IDPTYP      TO  UT1-IDPTYP                               
130400       MOVE 1A08-KDEKOHT     TO  UT1-KDEKOHT                              
130500       MOVE 1A08-IDFTG       TO  UT1-IDFTG                                
130600       MOVE 1A08-IDDC-SEND   TO  UT1-IDDC-SEND                            
130700       MOVE 1A08-IDDC-REC    TO  UT1-IDDC-REC                             
130800       MOVE ZERO             TO  UT1-IDDISTR                              
130900       MOVE ZERO             TO  UT1-IDKUNDNR                             
131000       MOVE ZERO             TO  UT1-IDFAKT                               
131100       MOVE SPACE            TO  UT1-KDFAKTYP                             
131200       MOVE ZERO             TO  UT1-DAFAKT                               
131300       MOVE ZERO             TO  UT1-IDORDNR7                             
131400       MOVE 1A08-IDARTNR     TO  UT1-IDARTNR                              
131500       MOVE 1A08-KDPRODSL    TO  UT1-KDPRODSL                             
131600       MOVE 1A08-KDPSLLOC    TO  UT1-KDPSLLOC                             
131700       MOVE ZERO             TO  UT1-KVLEVART                             
131800       MOVE ZERO             TO  UT1-PRARTNTO                             
131900       MOVE SPACE            TO  UT1-FLOVRLEV                             
132000       MOVE ZERO             TO  UT1-KDFRAKT                              
132100       MOVE ZERO             TO  UT1-SUFAKTRE                             
132200       MOVE ZERO             TO  UT1-PREMBHNT                             
132300       MOVE ZERO             TO  UT1-PRFRAKT                              
132400       MOVE ZERO             TO  UT1-PRFOERS                              
132500       MOVE ZERO             TO  UT1-PRMOMS                               
132600       MOVE ZERO             TO  UT1-PRLEGKST                             
132700       MOVE ZERO             TO  UT1-SUFKTTILL                            
132800       MOVE ZERO             TO  UT1-PRAVDRAG                             
132900       MOVE ZERO             TO  UT1-SUFKTBEL                             
133000       MOVE ZERO             TO  UT1-SUFKTUTL                             
133100       MOVE ZERO             TO  UT1-PRKURS                               
133200       MOVE ZERO             TO  UT1-IDRAPPNR                             
133300       MOVE ZERO             TO  UT1-IDKOLLI                              
133400       MOVE SPACE            TO  UT1-KDANMORS                             
133500       MOVE SPACE            TO  UT1-KDVALISO                             
133600       MOVE ZERO             TO  UT1-DAINLINL                             
133700       MOVE ZERO             TO  UT1-KVANTMOT                             
133800       MOVE ZERO             TO  UT1-KVSKROT                              
133900       MOVE 1A08-PRAVCOST    TO  UT1-PRAVCOST                             
134000       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
134100       MOVE ZERO             TO  UT1-KVLS-OLD                             
134200       MOVE SPACE            TO  UT1-FLSLUT                               
134300       MOVE ZERO             TO  UT1-REMARKUP                             
134400       MOVE ZERO             TO  UT1-IDKNOTNR                             
134500       MOVE ZERO             TO  UT1-DAKRENOT                             
134600       MOVE ZERO             TO  UT1-SUKREUTL                             
134700       MOVE ZERO             TO  UT1-SUKRENTO                             
134800       MOVE ZERO             TO  UT1-PRLANDCO                             
134900       MOVE ZERO             TO  UT1-SUKRENOT                             
135000       MOVE ZERO             TO  UT1-KVKREANT                             
135100       MOVE ZERO             TO  UT1-DARETILL                             
135200       MOVE ZERO             TO  UT1-KVLEVANM                             
135300       MOVE ZERO             TO  UT1-KVRETINL                             
135400       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
135500       MOVE ZERO             TO  UT1-IDKONTO                              
135600       MOVE SPACE            TO  UT1-IDKST                                
135700       MOVE 1A08-DAJUSTDA    TO  UT1-DAJUSTDA                             
135800       MOVE 1A08-KVJUSTKV    TO  UT1-KVJUSTKV                             
135900       MOVE 1A08-KDINVKAT    TO  UT1-KDINVKAT                             
136000       MOVE 1A08-TEINVANM    TO  UT1-TEINVANM                             
136100       MOVE ZERO             TO  UT1-IDPRODNR                             
136200       MOVE SPACE            TO  UT1-IDLEVNR                              
136300       MOVE ZERO             TO  UT1-KVBEART                              
136400       MOVE ZERO             TO  UT1-DAORDDAT                             
136500       MOVE ZERO             TO  UT1-PRARTBEU                             
136600       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
136700       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
136800       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
136900       MOVE SPACE            TO  UT1-IDFS                                 
137000       MOVE ZERO             TO  UT1-KVAVIS                               
137100       MOVE ZERO             TO  UT1-IDBYTRAP                             
137200       MOVE ZERO             TO  UT1-KVRETUR                              
137300       MOVE ZERO             TO  UT1-SUAVCOST                             
137400       MOVE SPACE            TO  UT1-IDUSER                               
137500       MOVE SPACE            TO  UT1-KDAVCOST                             
137600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
137700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
137800       MOVE ZERO             TO  UT1-KVEFRS                               
137900       MOVE ZERO             TO  UT1-KVLS                                 
138000       MOVE ZERO             TO  UT1-DAREGDAT                             
138100       MOVE ZERO             TO  UT1-DASTADAT                             
138200       MOVE ZERO             TO  UT1-PRKURS-SU                            
138300       MOVE ZERO             TO  UT1-PRKURS-SC                            
138400       MOVE ZERO             TO  UT1-PRKURS-UC                            
138500       MOVE ZERO             TO  UT1-PRKURS-CU                            
138510       PERFORM S11-WRITE-W56022A                                          
138600     END-IF                                                               
138700     IF IN1-IDPTYP = 'A09'                                                
138800       MOVE 1A09-IDPTYP      TO  UT1-IDPTYP                               
138900       MOVE 1A09-KDEKOHT     TO  UT1-KDEKOHT                              
139000       MOVE 1A09-IDFTG       TO  UT1-IDFTG                                
139100       MOVE 1A09-IDDC-SEND   TO  UT1-IDDC-SEND                            
139200       MOVE 1A09-IDDC-REC    TO  UT1-IDDC-REC                             
139300       MOVE 1A09-IDDISTR     TO  UT1-IDDISTR                              
139400       MOVE 1A09-IDKUNDNR    TO  UT1-IDKUNDNR                             
139500       MOVE 1A09-IDFAKT      TO  UT1-IDFAKT                               
139600       MOVE SPACE            TO  UT1-KDFAKTYP                             
139700       MOVE 1A09-DAFAKT      TO  UT1-DAFAKT                               
139800       MOVE 1A09-IDORDNR7    TO  UT1-IDORDNR7                             
139900       MOVE 1A09-IDARTNR     TO  UT1-IDARTNR                              
140000       MOVE 1A09-KDPRODSL    TO  UT1-KDPRODSL                             
140100       MOVE 1A09-KDPSLLOC    TO  UT1-KDPSLLOC                             
140200       MOVE 1A09-KVLEVART    TO  UT1-KVLEVART                             
140300       MOVE ZERO             TO  UT1-PRARTNTO                             
140400       MOVE SPACE            TO  UT1-FLOVRLEV                             
140500       MOVE ZERO             TO  UT1-KDFRAKT                              
140600       MOVE ZERO             TO  UT1-SUFAKTRE                             
140700       MOVE ZERO             TO  UT1-PREMBHNT                             
140800       MOVE ZERO             TO  UT1-PRFRAKT                              
140900       MOVE ZERO             TO  UT1-PRFOERS                              
141000       MOVE ZERO             TO  UT1-PRMOMS                               
141100       MOVE ZERO             TO  UT1-PRLEGKST                             
141200       MOVE ZERO             TO  UT1-SUFKTTILL                            
141300       MOVE ZERO             TO  UT1-PRAVDRAG                             
141400       MOVE ZERO             TO  UT1-SUFKTBEL                             
141500       MOVE ZERO             TO  UT1-SUFKTUTL                             
141600       MOVE ZERO             TO  UT1-PRKURS                               
141700       MOVE ZERO             TO  UT1-IDRAPPNR                             
141800       MOVE ZERO             TO  UT1-IDKOLLI                              
141900       MOVE SPACE            TO  UT1-KDANMORS                             
142000       MOVE SPACE            TO  UT1-KDVALISO                             
142100       MOVE ZERO             TO  UT1-DAINLINL                             
142200       MOVE ZERO             TO  UT1-KVANTMOT                             
142300       MOVE ZERO             TO  UT1-KVSKROT                              
142400       MOVE 1A09-PRAVCOST    TO  UT1-PRAVCOST                             
142500       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
142600       MOVE ZERO             TO  UT1-KVLS-OLD                             
142700       MOVE SPACE            TO  UT1-FLSLUT                               
142800       MOVE ZERO             TO  UT1-REMARKUP                             
142900       MOVE ZERO             TO  UT1-IDKNOTNR                             
143000       MOVE ZERO             TO  UT1-DAKRENOT                             
143100       MOVE ZERO             TO  UT1-SUKREUTL                             
143200       MOVE ZERO             TO  UT1-SUKRENTO                             
143300       MOVE ZERO             TO  UT1-PRLANDCO                             
143400       MOVE ZERO             TO  UT1-SUKRENOT                             
143500       MOVE ZERO             TO  UT1-KVKREANT                             
143600       MOVE ZERO             TO  UT1-DARETILL                             
143700       MOVE ZERO             TO  UT1-KVLEVANM                             
143800       MOVE ZERO             TO  UT1-KVRETINL                             
143900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
144000       MOVE ZERO             TO  UT1-IDKONTO                              
144100       MOVE SPACE            TO  UT1-IDKST                                
144200       MOVE ZERO             TO  UT1-DAJUSTDA                             
144300       MOVE ZERO             TO  UT1-KVJUSTKV                             
144400       MOVE ZERO             TO  UT1-KDINVKAT                             
144500       MOVE SPACE            TO  UT1-TEINVANM                             
144600       MOVE ZERO             TO  UT1-IDPRODNR                             
144700       MOVE SPACE            TO  UT1-IDLEVNR                              
144800       MOVE ZERO             TO  UT1-KVBEART                              
144900       MOVE ZERO             TO  UT1-DAORDDAT                             
145000       MOVE ZERO             TO  UT1-PRARTBEU                             
145100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
145200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
145300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
145400       MOVE SPACE            TO  UT1-IDFS                                 
145500       MOVE ZERO             TO  UT1-KVAVIS                               
145600       MOVE ZERO             TO  UT1-IDBYTRAP                             
145700       MOVE ZERO             TO  UT1-KVRETUR                              
145800       MOVE ZERO             TO  UT1-SUAVCOST                             
145900       MOVE SPACE            TO  UT1-IDUSER                               
146000       MOVE SPACE            TO  UT1-KDAVCOST                             
146100       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
146200       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
146300       MOVE ZERO             TO  UT1-KVEFRS                               
146400       MOVE ZERO             TO  UT1-KVLS                                 
146500       MOVE ZERO             TO  UT1-DAREGDAT                             
146600       MOVE ZERO             TO  UT1-DASTADAT                             
146700       MOVE ZERO             TO  UT1-PRKURS-SU                            
146800       MOVE ZERO             TO  UT1-PRKURS-SC                            
146900       MOVE ZERO             TO  UT1-PRKURS-UC                            
147000       MOVE ZERO             TO  UT1-PRKURS-CU                            
147010       PERFORM S11-WRITE-W56022A                                          
147100     END-IF                                                               
147200     IF IN1-IDPTYP = 'A10'                                                
147300       MOVE 1A10-IDPTYP      TO  UT1-IDPTYP                               
147400       MOVE 1A10-KDEKOHT     TO  UT1-KDEKOHT                              
147500       MOVE 1A10-IDFTG       TO  UT1-IDFTG                                
147600       MOVE 1A10-IDDC-SEND   TO  UT1-IDDC-SEND                            
147700       MOVE 1A10-IDDC-REC    TO  UT1-IDDC-REC                             
147800       MOVE 1A10-IDDISTR     TO  UT1-IDDISTR                              
147900       MOVE 1A10-IDKUNDNR    TO  UT1-IDKUNDNR                             
148000       MOVE ZERO             TO  UT1-IDFAKT                               
148100       MOVE SPACE            TO  UT1-KDFAKTYP                             
148200       MOVE ZERO             TO  UT1-DAFAKT                               
148300       MOVE 1A10-IDORDNR7    TO  UT1-IDORDNR7                             
148400       MOVE 1A10-IDARTNR     TO  UT1-IDARTNR                              
148500       MOVE 1A10-KDPRODSL    TO  UT1-KDPRODSL                             
148600       MOVE 1A10-KDPSLLOC    TO  UT1-KDPSLLOC                             
148700       MOVE 1A10-KVLEVART    TO  UT1-KVLEVART                             
148800       MOVE ZERO             TO  UT1-PRARTNTO                             
148900       MOVE SPACE            TO  UT1-FLOVRLEV                             
149000       MOVE ZERO             TO  UT1-KDFRAKT                              
149100       MOVE ZERO             TO  UT1-SUFAKTRE                             
149200       MOVE ZERO             TO  UT1-PREMBHNT                             
149300       MOVE ZERO             TO  UT1-PRFRAKT                              
149400       MOVE ZERO             TO  UT1-PRFOERS                              
149500       MOVE ZERO             TO  UT1-PRMOMS                               
149600       MOVE ZERO             TO  UT1-PRLEGKST                             
149700       MOVE ZERO             TO  UT1-SUFKTTILL                            
149800       MOVE ZERO             TO  UT1-PRAVDRAG                             
149900       MOVE ZERO             TO  UT1-SUFKTBEL                             
150000       MOVE ZERO             TO  UT1-SUFKTUTL                             
150100       MOVE ZERO             TO  UT1-PRKURS                               
150200       MOVE ZERO             TO  UT1-IDRAPPNR                             
150300       MOVE ZERO             TO  UT1-IDKOLLI                              
150400       MOVE SPACE            TO  UT1-KDANMORS                             
150500       MOVE SPACE            TO  UT1-KDVALISO                             
150600       MOVE ZERO             TO  UT1-DAINLINL                             
150700       MOVE ZERO             TO  UT1-KVANTMOT                             
150800       MOVE ZERO             TO  UT1-KVSKROT                              
150900       MOVE ZERO             TO  UT1-PRAVCOST                             
151000       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
151100       MOVE ZERO             TO  UT1-KVLS-OLD                             
151200       MOVE SPACE            TO  UT1-FLSLUT                               
151300       MOVE ZERO             TO  UT1-REMARKUP                             
151400       MOVE ZERO             TO  UT1-IDKNOTNR                             
151500       MOVE ZERO             TO  UT1-DAKRENOT                             
151600       MOVE ZERO             TO  UT1-SUKREUTL                             
151700       MOVE ZERO             TO  UT1-SUKRENTO                             
151800       MOVE ZERO             TO  UT1-PRLANDCO                             
151900       MOVE ZERO             TO  UT1-SUKRENOT                             
152000       MOVE ZERO             TO  UT1-KVKREANT                             
152100       MOVE ZERO             TO  UT1-DARETILL                             
152200       MOVE ZERO             TO  UT1-KVLEVANM                             
152300       MOVE ZERO             TO  UT1-KVRETINL                             
152400       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
152500       MOVE ZERO             TO  UT1-IDKONTO                              
152600       MOVE SPACE            TO  UT1-IDKST                                
152700       MOVE ZERO             TO  UT1-DAJUSTDA                             
152800       MOVE ZERO             TO  UT1-KVJUSTKV                             
152900       MOVE ZERO             TO  UT1-KDINVKAT                             
153000       MOVE SPACE            TO  UT1-TEINVANM                             
153100       MOVE 1A10-IDPRODNR    TO  UT1-IDPRODNR                             
153200       MOVE 1A10-IDLEVNR     TO  UT1-IDLEVNR                              
153300       MOVE 1A10-KVBEART     TO  UT1-KVBEART                              
153400       MOVE 1A10-DAORDDAT    TO  UT1-DAORDDAT                             
153500       MOVE 1A10-PRARTBEU    TO  UT1-PRARTBEU                             
153600       MOVE 1A10-PRARTNTO-GNB TO UT1-PRARTNTO-GNB                         
153700       MOVE 1A10-IDFAKT-GNB  TO  UT1-IDFAKT-GNB                           
153800       MOVE 1A10-DAFAKT-GNB  TO  UT1-DAFAKT-GNB                           
153900       MOVE SPACE            TO  UT1-IDFS                                 
154000       MOVE ZERO             TO  UT1-KVAVIS                               
154100       MOVE ZERO             TO  UT1-IDBYTRAP                             
154200       MOVE ZERO             TO  UT1-KVRETUR                              
154300       MOVE ZERO             TO  UT1-SUAVCOST                             
154400       MOVE SPACE            TO  UT1-IDUSER                               
154500       MOVE SPACE            TO  UT1-KDAVCOST                             
154600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
154700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
154800       MOVE ZERO             TO  UT1-KVEFRS                               
154900       MOVE ZERO             TO  UT1-KVLS                                 
155000       MOVE ZERO             TO  UT1-DAREGDAT                             
155100       MOVE ZERO             TO  UT1-DASTADAT                             
155200       MOVE ZERO             TO  UT1-PRKURS-SU                            
155300       MOVE ZERO             TO  UT1-PRKURS-SC                            
155400       MOVE ZERO             TO  UT1-PRKURS-UC                            
155500       MOVE ZERO             TO  UT1-PRKURS-CU                            
155510       PERFORM S11-WRITE-W56022A                                          
155600     END-IF                                                               
155700     IF IN1-IDPTYP = 'A11'                                                
155800       MOVE 1A11-IDPTYP      TO  UT1-IDPTYP                               
155900       MOVE 1A11-KDEKOHT     TO  UT1-KDEKOHT                              
156000       MOVE 1A11-IDFTG       TO  UT1-IDFTG                                
156100       MOVE 1A11-IDDC-SEND   TO  UT1-IDDC-SEND                            
156200       MOVE 1A11-IDDC-REC    TO  UT1-IDDC-REC                             
156300       MOVE ZERO             TO  UT1-IDDISTR                              
156400       MOVE ZERO             TO  UT1-IDKUNDNR                             
156500       MOVE ZERO             TO  UT1-IDFAKT                               
156600       MOVE SPACE            TO  UT1-KDFAKTYP                             
156700       MOVE ZERO             TO  UT1-DAFAKT                               
156800       MOVE 1A11-IDORDNR7    TO  UT1-IDORDNR7                             
156900       MOVE 1A11-IDARTNR     TO  UT1-IDARTNR                              
157000       MOVE 1A11-KDPRODSL    TO  UT1-KDPRODSL                             
157100       MOVE 1A11-KDPSLLOC    TO  UT1-KDPSLLOC                             
157200       MOVE ZERO             TO  UT1-KVLEVART                             
157300       MOVE ZERO             TO  UT1-PRARTNTO                             
157400       MOVE SPACE            TO  UT1-FLOVRLEV                             
157500       MOVE ZERO             TO  UT1-KDFRAKT                              
157600       MOVE ZERO             TO  UT1-SUFAKTRE                             
157700       MOVE ZERO             TO  UT1-PREMBHNT                             
157800       MOVE ZERO             TO  UT1-PRFRAKT                              
157900       MOVE ZERO             TO  UT1-PRFOERS                              
158000       MOVE ZERO             TO  UT1-PRMOMS                               
158100       MOVE ZERO             TO  UT1-PRLEGKST                             
158200       MOVE ZERO             TO  UT1-SUFKTTILL                            
158300       MOVE ZERO             TO  UT1-PRAVDRAG                             
158400       MOVE ZERO             TO  UT1-SUFKTBEL                             
158500       MOVE ZERO             TO  UT1-SUFKTUTL                             
158600       MOVE ZERO             TO  UT1-PRKURS                               
158700       MOVE ZERO             TO  UT1-IDRAPPNR                             
158800       MOVE ZERO             TO  UT1-IDKOLLI                              
158900       MOVE SPACE            TO  UT1-KDANMORS                             
159000       MOVE SPACE            TO  UT1-KDVALISO                             
159100       MOVE 1A11-DAINLINL    TO  UT1-DAINLINL                             
159200       MOVE 1A11-KVANTMOT    TO  UT1-KVANTMOT                             
159300       MOVE ZERO             TO  UT1-KVSKROT                              
159400       MOVE 1A11-PRAVCOST    TO  UT1-PRAVCOST                             
159500       MOVE 1A11-PRAVCOST-OLD TO UT1-PRAVCOST-OLD                         
159600       MOVE 1A11-KVLS-OLD    TO  UT1-KVLS-OLD                             
159700       MOVE SPACE            TO  UT1-FLSLUT                               
159800       MOVE 1A11-REMARKUP    TO  UT1-REMARKUP                             
159900       MOVE ZERO             TO  UT1-IDKNOTNR                             
160000       MOVE ZERO             TO  UT1-DAKRENOT                             
160100       MOVE ZERO             TO  UT1-SUKREUTL                             
160200       MOVE ZERO             TO  UT1-SUKRENTO                             
160300       MOVE ZERO             TO  UT1-PRLANDCO                             
160400       MOVE ZERO             TO  UT1-SUKRENOT                             
160500       MOVE ZERO             TO  UT1-KVKREANT                             
160600       MOVE ZERO             TO  UT1-DARETILL                             
160700       MOVE ZERO             TO  UT1-KVLEVANM                             
160800       MOVE ZERO             TO  UT1-KVRETINL                             
160900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
161000       MOVE ZERO             TO  UT1-IDKONTO                              
161100       MOVE SPACE            TO  UT1-IDKST                                
161200       MOVE ZERO             TO  UT1-DAJUSTDA                             
161300       MOVE ZERO             TO  UT1-KVJUSTKV                             
161400       MOVE ZERO             TO  UT1-KDINVKAT                             
161500       MOVE SPACE            TO  UT1-TEINVANM                             
161600       MOVE ZERO             TO  UT1-IDPRODNR                             
161700       MOVE 1A11-IDLEVNR     TO  UT1-IDLEVNR                              
161800       MOVE ZERO             TO  UT1-KVBEART                              
161900       MOVE ZERO             TO  UT1-DAORDDAT                             
162000       MOVE 1A11-PRARTBEU    TO  UT1-PRARTBEU                             
162100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
162200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
162300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
162400       MOVE 1A11-IDFS        TO  UT1-IDFS                                 
162500       MOVE 1A11-KVAVIS      TO  UT1-KVAVIS                               
162600       MOVE ZERO             TO  UT1-IDBYTRAP                             
162700       MOVE ZERO             TO  UT1-KVRETUR                              
162800       MOVE ZERO             TO  UT1-SUAVCOST                             
162900       MOVE SPACE            TO  UT1-IDUSER                               
163000       MOVE SPACE            TO  UT1-KDAVCOST                             
163100       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
163200       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
163300       MOVE ZERO             TO  UT1-KVEFRS                               
163400       MOVE ZERO             TO  UT1-KVLS                                 
163500       MOVE ZERO             TO  UT1-DAREGDAT                             
163600       MOVE ZERO             TO  UT1-DASTADAT                             
163700       MOVE ZERO             TO  UT1-PRKURS-SU                            
163800       MOVE ZERO             TO  UT1-PRKURS-SC                            
163900       MOVE ZERO             TO  UT1-PRKURS-UC                            
164000       MOVE ZERO             TO  UT1-PRKURS-CU                            
164010       PERFORM S11-WRITE-W56022A                                          
164100     END-IF                                                               
164200     IF IN1-IDPTYP = 'A12'                                                
164300       MOVE 1A12-IDPTYP      TO  UT1-IDPTYP                               
164400       MOVE 1A12-KDEKOHT     TO  UT1-KDEKOHT                              
164500       MOVE 1A12-IDFTG       TO  UT1-IDFTG                                
164600       MOVE 1A12-IDDC-SEND   TO  UT1-IDDC-SEND                            
164700       MOVE 1A12-IDDC-REC    TO  UT1-IDDC-REC                             
164800       MOVE 1A12-IDDISTR     TO  UT1-IDDISTR                              
164900       MOVE 1A12-IDKUNDNR    TO  UT1-IDKUNDNR                             
165000       MOVE ZERO             TO  UT1-IDFAKT                               
165100       MOVE SPACE            TO  UT1-KDFAKTYP                             
165200       MOVE ZERO             TO  UT1-DAFAKT                               
165300       MOVE ZERO             TO  UT1-IDORDNR7                             
165400       MOVE 1A12-IDARTNR     TO  UT1-IDARTNR                              
165500       MOVE 1A12-KDPRODSL    TO  UT1-KDPRODSL                             
165600       MOVE 1A12-KDPSLLOC    TO  UT1-KDPSLLOC                             
165700       MOVE ZERO             TO  UT1-KVLEVART                             
165800       MOVE ZERO             TO  UT1-PRARTNTO                             
165900       MOVE SPACE            TO  UT1-FLOVRLEV                             
166000       MOVE ZERO             TO  UT1-KDFRAKT                              
166100       MOVE ZERO             TO  UT1-SUFAKTRE                             
166200       MOVE ZERO             TO  UT1-PREMBHNT                             
166300       MOVE ZERO             TO  UT1-PRFRAKT                              
166400       MOVE ZERO             TO  UT1-PRFOERS                              
166500       MOVE ZERO             TO  UT1-PRMOMS                               
166600       MOVE ZERO             TO  UT1-PRLEGKST                             
166700       MOVE ZERO             TO  UT1-SUFKTTILL                            
166800       MOVE ZERO             TO  UT1-PRAVDRAG                             
166900       MOVE ZERO             TO  UT1-SUFKTBEL                             
167000       MOVE ZERO             TO  UT1-SUFKTUTL                             
167100       MOVE ZERO             TO  UT1-PRKURS                               
167200       MOVE 1A12-IDRAPPNR    TO  UT1-IDRAPPNR                             
167300       MOVE ZERO             TO  UT1-IDKOLLI                              
167400       MOVE 1A12-KDANMORS    TO  UT1-KDANMORS                             
167500       MOVE SPACE            TO  UT1-KDVALISO                             
167600       MOVE ZERO             TO  UT1-DAINLINL                             
167700       MOVE ZERO             TO  UT1-KVANTMOT                             
167800       MOVE ZERO             TO  UT1-KVSKROT                              
167900       MOVE 1A12-PRAVCOST    TO  UT1-PRAVCOST                             
168000       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
168100       MOVE ZERO             TO  UT1-KVLS-OLD                             
168200       MOVE SPACE            TO  UT1-FLSLUT                               
168300       MOVE ZERO             TO  UT1-REMARKUP                             
168400       MOVE ZERO             TO  UT1-IDKNOTNR                             
168500       MOVE ZERO             TO  UT1-DAKRENOT                             
168600       MOVE ZERO             TO  UT1-SUKREUTL                             
168700       MOVE ZERO             TO  UT1-SUKRENTO                             
168800       MOVE ZERO             TO  UT1-PRLANDCO                             
168900       MOVE ZERO             TO  UT1-SUKRENOT                             
169000       MOVE ZERO             TO  UT1-KVKREANT                             
169100       MOVE ZERO             TO  UT1-DARETILL                             
169200       MOVE ZERO             TO  UT1-KVLEVANM                             
169300       MOVE ZERO             TO  UT1-KVRETINL                             
169400       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
169500       MOVE ZERO             TO  UT1-IDKONTO                              
169600       MOVE SPACE            TO  UT1-IDKST                                
169700       MOVE 1A12-DAJUSTDA    TO  UT1-DAJUSTDA                             
169800       MOVE 1A12-KVJUSTKV    TO  UT1-KVJUSTKV                             
169900       MOVE ZERO             TO  UT1-KDINVKAT                             
170000       MOVE SPACE            TO  UT1-TEINVANM                             
170100       MOVE ZERO             TO  UT1-IDPRODNR                             
170200       MOVE SPACE            TO  UT1-IDLEVNR                              
170300       MOVE ZERO             TO  UT1-KVBEART                              
170400       MOVE ZERO             TO  UT1-DAORDDAT                             
170500       MOVE ZERO             TO  UT1-PRARTBEU                             
170600       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
170700       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
170800       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
170900       MOVE SPACE            TO  UT1-IDFS                                 
171000       MOVE ZERO             TO  UT1-KVAVIS                               
171100       MOVE ZERO             TO  UT1-IDBYTRAP                             
171200       MOVE ZERO             TO  UT1-KVRETUR                              
171300       MOVE ZERO             TO  UT1-SUAVCOST                             
171400       MOVE SPACE            TO  UT1-IDUSER                               
171500       MOVE SPACE            TO  UT1-KDAVCOST                             
171600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
171700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
171800       MOVE ZERO             TO  UT1-KVEFRS                               
171900       MOVE ZERO             TO  UT1-KVLS                                 
172000       MOVE ZERO             TO  UT1-DAREGDAT                             
172100       MOVE ZERO             TO  UT1-DASTADAT                             
172200       MOVE ZERO             TO  UT1-PRKURS-SU                            
172300       MOVE ZERO             TO  UT1-PRKURS-SC                            
172400       MOVE ZERO             TO  UT1-PRKURS-UC                            
172500       MOVE ZERO             TO  UT1-PRKURS-CU                            
172510       PERFORM S11-WRITE-W56022A                                          
172600     END-IF                                                               
172700     IF IN1-IDPTYP = 'A13'                                                
172800       MOVE 1A13-IDPTYP      TO  UT1-IDPTYP                               
172900       MOVE 1A13-KDEKOHT     TO  UT1-KDEKOHT                              
173000       MOVE 1A13-IDFTG       TO  UT1-IDFTG                                
173100       MOVE 1A13-IDDC-SEND   TO  UT1-IDDC-SEND                            
173200       MOVE 1A13-IDDC-REC    TO  UT1-IDDC-REC                             
173300       MOVE 1A13-IDDISTR     TO  UT1-IDDISTR                              
173400       MOVE 1A13-IDKUNDNR    TO  UT1-IDKUNDNR                             
173500       MOVE 1A13-IDFAKT      TO  UT1-IDFAKT                               
173600       MOVE SPACE            TO  UT1-KDFAKTYP                             
173700       MOVE 1A13-DAFAKT      TO  UT1-DAFAKT                               
173800       MOVE 1A13-IDORDNR7    TO  UT1-IDORDNR7                             
173900       MOVE 1A13-IDARTNR     TO  UT1-IDARTNR                              
174000       MOVE 1A13-KDPRODSL    TO  UT1-KDPRODSL                             
174100       MOVE 1A13-KDPSLLOC    TO  UT1-KDPSLLOC                             
174200       MOVE 1A13-KVLEVART    TO  UT1-KVLEVART                             
174300       MOVE 1A13-PRARTNTO    TO  UT1-PRARTNTO                             
174400       MOVE SPACE            TO  UT1-FLOVRLEV                             
174500       MOVE ZERO             TO  UT1-KDFRAKT                              
174600       MOVE ZERO             TO  UT1-SUFAKTRE                             
174700       MOVE ZERO             TO  UT1-PREMBHNT                             
174800       MOVE ZERO             TO  UT1-PRFRAKT                              
174900       MOVE ZERO             TO  UT1-PRFOERS                              
175000       MOVE ZERO             TO  UT1-PRMOMS                               
175100       MOVE ZERO             TO  UT1-PRLEGKST                             
175200       MOVE ZERO             TO  UT1-SUFKTTILL                            
175300       MOVE ZERO             TO  UT1-PRAVDRAG                             
175400       MOVE ZERO             TO  UT1-SUFKTBEL                             
175500       MOVE ZERO             TO  UT1-SUFKTUTL                             
175600       MOVE 1A13-PRKURS      TO  UT1-PRKURS                               
175700       MOVE ZERO             TO  UT1-IDRAPPNR                             
175800       MOVE ZERO             TO  UT1-IDKOLLI                              
175900       MOVE SPACE            TO  UT1-KDANMORS                             
176000       MOVE SPACE            TO  UT1-KDVALISO                             
176100       MOVE ZERO             TO  UT1-DAINLINL                             
176200       MOVE ZERO             TO  UT1-KVANTMOT                             
176300       MOVE ZERO             TO  UT1-KVSKROT                              
176400       MOVE 1A13-PRAVCOST    TO  UT1-PRAVCOST                             
176500       MOVE 1A13-PRAVCOST-OLD TO UT1-PRAVCOST-OLD                         
176600       MOVE 1A13-KVLS-OLD    TO  UT1-KVLS-OLD                             
176700       MOVE SPACE            TO  UT1-FLSLUT                               
176800       MOVE 1A13-REMARKUP    TO  UT1-REMARKUP                             
176900       MOVE ZERO             TO  UT1-IDKNOTNR                             
177000       MOVE ZERO             TO  UT1-DAKRENOT                             
177100       MOVE ZERO             TO  UT1-SUKREUTL                             
177200       MOVE ZERO             TO  UT1-SUKRENTO                             
177300       MOVE ZERO             TO  UT1-PRLANDCO                             
177400       MOVE ZERO             TO  UT1-SUKRENOT                             
177500       MOVE ZERO             TO  UT1-KVKREANT                             
177600       MOVE ZERO             TO  UT1-DARETILL                             
177700       MOVE ZERO             TO  UT1-KVLEVANM                             
177800       MOVE ZERO             TO  UT1-KVRETINL                             
177900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
178000       MOVE ZERO             TO  UT1-IDKONTO                              
178100       MOVE SPACE            TO  UT1-IDKST                                
178200       MOVE ZERO             TO  UT1-DAJUSTDA                             
178300       MOVE ZERO             TO  UT1-KVJUSTKV                             
178400       MOVE ZERO             TO  UT1-KDINVKAT                             
178500       MOVE SPACE            TO  UT1-TEINVANM                             
178600       MOVE ZERO             TO  UT1-IDPRODNR                             
178700       MOVE SPACE            TO  UT1-IDLEVNR                              
178800       MOVE ZERO             TO  UT1-KVBEART                              
178900       MOVE ZERO             TO  UT1-DAORDDAT                             
179000       MOVE ZERO             TO  UT1-PRARTBEU                             
179100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
179200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
179300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
179400       MOVE SPACE            TO  UT1-IDFS                                 
179500       MOVE ZERO             TO  UT1-KVAVIS                               
179600       MOVE ZERO             TO  UT1-IDBYTRAP                             
179700       MOVE ZERO             TO  UT1-KVRETUR                              
179800       MOVE ZERO             TO  UT1-SUAVCOST                             
179900       MOVE SPACE            TO  UT1-IDUSER                               
180000       MOVE SPACE            TO  UT1-KDAVCOST                             
180100       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
180200       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
180300       MOVE ZERO             TO  UT1-KVEFRS                               
180400       MOVE ZERO             TO  UT1-KVLS                                 
180500       MOVE ZERO             TO  UT1-DAREGDAT                             
180600       MOVE ZERO             TO  UT1-DASTADAT                             
180700       MOVE ZERO             TO  UT1-PRKURS-SU                            
180800       MOVE ZERO             TO  UT1-PRKURS-SC                            
180900       MOVE ZERO             TO  UT1-PRKURS-UC                            
181000       MOVE ZERO             TO  UT1-PRKURS-CU                            
181010       PERFORM S11-WRITE-W56022A                                          
181100     END-IF                                                               
181200     IF IN1-IDPTYP = 'A14'                                                
181300       MOVE 1A14-IDPTYP      TO  UT1-IDPTYP                               
181400       MOVE 1A14-KDEKOHT     TO  UT1-KDEKOHT                              
181500       MOVE 1A14-IDFTG       TO  UT1-IDFTG                                
181600       MOVE 1A14-IDDC-SEND   TO  UT1-IDDC-SEND                            
181700       MOVE 1A14-IDDC-REC    TO  UT1-IDDC-REC                             
181800       MOVE 1A14-IDDISTR     TO  UT1-IDDISTR                              
181900       MOVE 1A14-IDKUNDNR    TO  UT1-IDKUNDNR                             
182000       MOVE ZERO             TO  UT1-IDFAKT                               
182100       MOVE SPACE            TO  UT1-KDFAKTYP                             
182200       MOVE ZERO             TO  UT1-DAFAKT                               
182300       MOVE ZERO             TO  UT1-IDORDNR7                             
182400       MOVE 1A14-IDARTNR     TO  UT1-IDARTNR                              
182500       MOVE 1A14-KDPRODSL    TO  UT1-KDPRODSL                             
182600       MOVE 1A14-KDPSLLOC    TO  UT1-KDPSLLOC                             
182700       MOVE ZERO             TO  UT1-KVLEVART                             
182800       MOVE ZERO             TO  UT1-PRARTNTO                             
182900       MOVE SPACE            TO  UT1-FLOVRLEV                             
183000       MOVE ZERO             TO  UT1-KDFRAKT                              
183100       MOVE ZERO             TO  UT1-SUFAKTRE                             
183200       MOVE ZERO             TO  UT1-PREMBHNT                             
183300       MOVE ZERO             TO  UT1-PRFRAKT                              
183400       MOVE ZERO             TO  UT1-PRFOERS                              
183500       MOVE ZERO             TO  UT1-PRMOMS                               
183600       MOVE ZERO             TO  UT1-PRLEGKST                             
183700       MOVE ZERO             TO  UT1-SUFKTTILL                            
183800       MOVE ZERO             TO  UT1-PRAVDRAG                             
183900       MOVE ZERO             TO  UT1-SUFKTBEL                             
184000       MOVE ZERO             TO  UT1-SUFKTUTL                             
184100       MOVE ZERO             TO  UT1-PRKURS                               
184200       MOVE ZERO             TO  UT1-IDRAPPNR                             
184300       MOVE ZERO             TO  UT1-IDKOLLI                              
184400       MOVE SPACE            TO  UT1-KDANMORS                             
184500       MOVE SPACE            TO  UT1-KDVALISO                             
184600       MOVE 1A14-DAINLINL    TO  UT1-DAINLINL                             
184700       MOVE ZERO             TO  UT1-KVANTMOT                             
184800       MOVE ZERO             TO  UT1-KVSKROT                              
184900       MOVE 1A14-PRAVCOST    TO  UT1-PRAVCOST                             
185000       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
185100       MOVE ZERO             TO  UT1-KVLS-OLD                             
185200       MOVE SPACE            TO  UT1-FLSLUT                               
185300       MOVE ZERO             TO  UT1-REMARKUP                             
185400       MOVE ZERO             TO  UT1-IDKNOTNR                             
185500       MOVE ZERO             TO  UT1-DAKRENOT                             
185600       MOVE ZERO             TO  UT1-SUKREUTL                             
185700       MOVE ZERO             TO  UT1-SUKRENTO                             
185800       MOVE ZERO             TO  UT1-PRLANDCO                             
185900       MOVE ZERO             TO  UT1-SUKRENOT                             
186000       MOVE ZERO             TO  UT1-KVKREANT                             
186100       MOVE ZERO             TO  UT1-DARETILL                             
186200       MOVE ZERO             TO  UT1-KVLEVANM                             
186300       MOVE ZERO             TO  UT1-KVRETINL                             
186400       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
186500       MOVE ZERO             TO  UT1-IDKONTO                              
186600       MOVE SPACE            TO  UT1-IDKST                                
186700       MOVE ZERO             TO  UT1-DAJUSTDA                             
186800       MOVE ZERO             TO  UT1-KVJUSTKV                             
186900       MOVE ZERO             TO  UT1-KDINVKAT                             
187000       MOVE SPACE            TO  UT1-TEINVANM                             
187100       MOVE ZERO             TO  UT1-IDPRODNR                             
187200       MOVE SPACE            TO  UT1-IDLEVNR                              
187300       MOVE ZERO             TO  UT1-KVBEART                              
187400       MOVE ZERO             TO  UT1-DAORDDAT                             
187500       MOVE ZERO             TO  UT1-PRARTBEU                             
187600       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
187700       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
187800       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
187900       MOVE SPACE            TO  UT1-IDFS                                 
188000       MOVE ZERO             TO  UT1-KVAVIS                               
188100       MOVE 1A14-IDBYTRAP    TO  UT1-IDBYTRAP                             
188200       MOVE 1A14-KVRETUR     TO  UT1-KVRETUR                              
188300       MOVE ZERO             TO  UT1-SUAVCOST                             
188400       MOVE SPACE            TO  UT1-IDUSER                               
188500       MOVE SPACE            TO  UT1-KDAVCOST                             
188600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
188700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
188800       MOVE ZERO             TO  UT1-KVEFRS                               
188900       MOVE ZERO             TO  UT1-KVLS                                 
189000       MOVE ZERO             TO  UT1-DAREGDAT                             
189100       MOVE ZERO             TO  UT1-DASTADAT                             
189200       MOVE ZERO             TO  UT1-PRKURS-SU                            
189300       MOVE ZERO             TO  UT1-PRKURS-SC                            
189400       MOVE ZERO             TO  UT1-PRKURS-UC                            
189500       MOVE ZERO             TO  UT1-PRKURS-CU                            
189510       PERFORM S11-WRITE-W56022A                                          
189600     END-IF                                                               
189700     IF IN1-IDPTYP = 'A15'                                                
189800       MOVE 1A15-IDPTYP      TO  UT1-IDPTYP                               
189900       MOVE 1A15-KDEKOHT     TO  UT1-KDEKOHT                              
190000       MOVE 1A15-IDFTG       TO  UT1-IDFTG                                
190100       MOVE 1A15-IDDC-SEND   TO  UT1-IDDC-SEND                            
190200       MOVE 1A15-IDDC-REC    TO  UT1-IDDC-REC                             
190300       MOVE 1A15-IDDISTR     TO  UT1-IDDISTR                              
190400       MOVE 1A15-IDKUNDNR    TO  UT1-IDKUNDNR                             
190500       MOVE 1A15-IDFAKT      TO  UT1-IDFAKT                               
190600       MOVE SPACE            TO  UT1-KDFAKTYP                             
190700       MOVE 1A15-DAFAKT      TO  UT1-DAFAKT                               
190800       MOVE ZERO             TO  UT1-IDORDNR7                             
190900       MOVE 1A15-IDARTNR     TO  UT1-IDARTNR                              
191000       MOVE 1A15-KDPRODSL    TO  UT1-KDPRODSL                             
191100       MOVE 1A15-KDPSLLOC    TO  UT1-KDPSLLOC                             
191200       MOVE 1A15-KVLEVART    TO  UT1-KVLEVART                             
191300       MOVE ZERO             TO  UT1-PRARTNTO                             
191400       MOVE SPACE            TO  UT1-FLOVRLEV                             
191500       MOVE ZERO             TO  UT1-KDFRAKT                              
191600       MOVE ZERO             TO  UT1-SUFAKTRE                             
191700       MOVE ZERO             TO  UT1-PREMBHNT                             
191800       MOVE ZERO             TO  UT1-PRFRAKT                              
191900       MOVE ZERO             TO  UT1-PRFOERS                              
192000       MOVE ZERO             TO  UT1-PRMOMS                               
192100       MOVE ZERO             TO  UT1-PRLEGKST                             
192200       MOVE ZERO             TO  UT1-SUFKTTILL                            
192300       MOVE ZERO             TO  UT1-PRAVDRAG                             
192400       MOVE ZERO             TO  UT1-SUFKTBEL                             
192500       MOVE ZERO             TO  UT1-SUFKTUTL                             
192600       MOVE ZERO             TO  UT1-PRKURS                               
192700       MOVE ZERO             TO  UT1-IDRAPPNR                             
192800       MOVE ZERO             TO  UT1-IDKOLLI                              
192900       MOVE SPACE            TO  UT1-KDANMORS                             
193000       MOVE SPACE            TO  UT1-KDVALISO                             
193100       MOVE ZERO             TO  UT1-DAINLINL                             
193200       MOVE ZERO             TO  UT1-KVANTMOT                             
193300       MOVE ZERO             TO  UT1-KVSKROT                              
193400       MOVE 1A15-PRAVCOST    TO  UT1-PRAVCOST                             
193500       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
193600       MOVE ZERO             TO  UT1-KVLS-OLD                             
193700       MOVE SPACE            TO  UT1-FLSLUT                               
193800       MOVE ZERO             TO  UT1-REMARKUP                             
193900       MOVE ZERO             TO  UT1-IDKNOTNR                             
194000       MOVE ZERO             TO  UT1-DAKRENOT                             
194100       MOVE ZERO             TO  UT1-SUKREUTL                             
194200       MOVE ZERO             TO  UT1-SUKRENTO                             
194300       MOVE ZERO             TO  UT1-PRLANDCO                             
194400       MOVE ZERO             TO  UT1-SUKRENOT                             
194500       MOVE ZERO             TO  UT1-KVKREANT                             
194600       MOVE ZERO             TO  UT1-DARETILL                             
194700       MOVE ZERO             TO  UT1-KVLEVANM                             
194800       MOVE ZERO             TO  UT1-KVRETINL                             
194900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
195000       MOVE ZERO             TO  UT1-IDKONTO                              
195100       MOVE SPACE            TO  UT1-IDKST                                
195200       MOVE ZERO             TO  UT1-DAJUSTDA                             
195300       MOVE ZERO             TO  UT1-KVJUSTKV                             
195400       MOVE ZERO             TO  UT1-KDINVKAT                             
195500       MOVE SPACE            TO  UT1-TEINVANM                             
195600       MOVE ZERO             TO  UT1-IDPRODNR                             
195700       MOVE SPACE            TO  UT1-IDLEVNR                              
195800       MOVE ZERO             TO  UT1-KVBEART                              
195900       MOVE ZERO             TO  UT1-DAORDDAT                             
196000       MOVE ZERO             TO  UT1-PRARTBEU                             
196100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
196200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
196300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
196400       MOVE SPACE            TO  UT1-IDFS                                 
196500       MOVE ZERO             TO  UT1-KVAVIS                               
196600       MOVE ZERO             TO  UT1-IDBYTRAP                             
196700       MOVE ZERO             TO  UT1-KVRETUR                              
196800       MOVE ZERO             TO  UT1-SUAVCOST                             
196900       MOVE SPACE            TO  UT1-IDUSER                               
197000       MOVE SPACE            TO  UT1-KDAVCOST                             
197100       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
197200       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
197300       MOVE ZERO             TO  UT1-KVEFRS                               
197400       MOVE ZERO             TO  UT1-KVLS                                 
197500       MOVE ZERO             TO  UT1-DAREGDAT                             
197600       MOVE ZERO             TO  UT1-DASTADAT                             
197700       MOVE ZERO             TO  UT1-PRKURS-SU                            
197800       MOVE ZERO             TO  UT1-PRKURS-SC                            
197900       MOVE ZERO             TO  UT1-PRKURS-UC                            
198000       MOVE ZERO             TO  UT1-PRKURS-CU                            
198010       PERFORM S11-WRITE-W56022A                                          
198100     END-IF                                                               
198200     IF IN1-IDPTYP = 'A16'                                                
198300       MOVE 1A16-IDPTYP      TO  UT1-IDPTYP                               
198400       MOVE 1A16-KDEKOHT     TO  UT1-KDEKOHT                              
198500       MOVE 1A16-IDFTG       TO  UT1-IDFTG                                
198600       MOVE 1A16-IDDC-SEND   TO  UT1-IDDC-SEND                            
198700       MOVE 1A16-IDDC-REC    TO  UT1-IDDC-REC                             
198800       MOVE ZERO             TO  UT1-IDDISTR                              
198900       MOVE ZERO             TO  UT1-IDKUNDNR                             
199000       MOVE ZERO             TO  UT1-IDFAKT                               
199100       MOVE SPACE            TO  UT1-KDFAKTYP                             
199200       MOVE ZERO             TO  UT1-DAFAKT                               
199300       MOVE ZERO             TO  UT1-IDORDNR7                             
199400       MOVE 1A16-IDARTNR     TO  UT1-IDARTNR                              
199500       MOVE 1A16-KDPRODSL    TO  UT1-KDPRODSL                             
199600       MOVE 1A16-KDPSLLOC    TO  UT1-KDPSLLOC                             
199700       MOVE ZERO             TO  UT1-KVLEVART                             
199800       MOVE ZERO             TO  UT1-PRARTNTO                             
199900       MOVE SPACE            TO  UT1-FLOVRLEV                             
200000       MOVE ZERO             TO  UT1-KDFRAKT                              
200100       MOVE ZERO             TO  UT1-SUFAKTRE                             
200200       MOVE ZERO             TO  UT1-PREMBHNT                             
200300       MOVE ZERO             TO  UT1-PRFRAKT                              
200400       MOVE ZERO             TO  UT1-PRFOERS                              
200500       MOVE ZERO             TO  UT1-PRMOMS                               
200600       MOVE ZERO             TO  UT1-PRLEGKST                             
200700       MOVE ZERO             TO  UT1-SUFKTTILL                            
200800       MOVE ZERO             TO  UT1-PRAVDRAG                             
200900       MOVE ZERO             TO  UT1-SUFKTBEL                             
201000       MOVE ZERO             TO  UT1-SUFKTUTL                             
201100       MOVE ZERO             TO  UT1-PRKURS                               
201200       MOVE ZERO             TO  UT1-IDRAPPNR                             
201300       MOVE ZERO             TO  UT1-IDKOLLI                              
201400       MOVE SPACE            TO  UT1-KDANMORS                             
201500       MOVE SPACE            TO  UT1-KDVALISO                             
201600       MOVE ZERO             TO  UT1-DAINLINL                             
201700       MOVE ZERO             TO  UT1-KVANTMOT                             
201800       MOVE ZERO             TO  UT1-KVSKROT                              
201900       MOVE 1A16-PRAVCOST    TO  UT1-PRAVCOST                             
202000       MOVE 1A16-PRAVCOST-OLD TO UT1-PRAVCOST-OLD                         
202100       MOVE ZERO             TO  UT1-KVLS-OLD                             
202200       MOVE SPACE            TO  UT1-FLSLUT                               
202300       MOVE ZERO             TO  UT1-REMARKUP                             
202400       MOVE ZERO             TO  UT1-IDKNOTNR                             
202500       MOVE ZERO             TO  UT1-DAKRENOT                             
202600       MOVE ZERO             TO  UT1-SUKREUTL                             
202700       MOVE ZERO             TO  UT1-SUKRENTO                             
202800       MOVE ZERO             TO  UT1-PRLANDCO                             
202900       MOVE ZERO             TO  UT1-SUKRENOT                             
203000       MOVE ZERO             TO  UT1-KVKREANT                             
203100       MOVE ZERO             TO  UT1-DARETILL                             
203200       MOVE ZERO             TO  UT1-KVLEVANM                             
203300       MOVE ZERO             TO  UT1-KVRETINL                             
203400       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
203500       MOVE ZERO             TO  UT1-IDKONTO                              
203600       MOVE SPACE            TO  UT1-IDKST                                
203700       MOVE 1A16-DAJUSTDA    TO  UT1-DAJUSTDA                             
203800       MOVE ZERO             TO  UT1-KVJUSTKV                             
203900       MOVE ZERO             TO  UT1-KDINVKAT                             
204000       MOVE SPACE            TO  UT1-TEINVANM                             
204100       MOVE ZERO             TO  UT1-IDPRODNR                             
204200       MOVE SPACE            TO  UT1-IDLEVNR                              
204300       MOVE ZERO             TO  UT1-KVBEART                              
204400       MOVE ZERO             TO  UT1-DAORDDAT                             
204500       MOVE ZERO             TO  UT1-PRARTBEU                             
204600       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
204700       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
204800       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
204900       MOVE 1A16-IDFS        TO  UT1-IDFS                                 
205000       MOVE ZERO             TO  UT1-KVAVIS                               
205100       MOVE ZERO             TO  UT1-IDBYTRAP                             
205200       MOVE ZERO             TO  UT1-KVRETUR                              
205300       MOVE 1A16-SUAVCOST    TO  UT1-SUAVCOST                             
205400       MOVE 1A16-IDUSER      TO  UT1-IDUSER                               
205500       MOVE 1A16-KDAVCOST    TO  UT1-KDAVCOST                             
205600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
205700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
205800       MOVE ZERO             TO  UT1-KVEFRS                               
205900       MOVE 1A16-KVLS        TO  UT1-KVLS                                 
206000       MOVE ZERO             TO  UT1-DAREGDAT                             
206100       MOVE ZERO             TO  UT1-DASTADAT                             
206200       MOVE ZERO             TO  UT1-PRKURS-SU                            
206300       MOVE ZERO             TO  UT1-PRKURS-SC                            
206400       MOVE ZERO             TO  UT1-PRKURS-UC                            
206500       MOVE ZERO             TO  UT1-PRKURS-CU                            
206510       PERFORM S11-WRITE-W56022A                                          
206600     END-IF                                                               
206700     IF IN1-IDPTYP = 'A17'                                                
206800       MOVE 1A17-IDPTYP      TO  UT1-IDPTYP                               
206900       MOVE 1A17-KDEKOHT     TO  UT1-KDEKOHT                              
207000       MOVE 1A17-IDFTG       TO  UT1-IDFTG                                
207100       MOVE 1A17-IDDC-SEND   TO  UT1-IDDC-SEND                            
207200       MOVE 1A17-IDDC-REC    TO  UT1-IDDC-REC                             
207300       MOVE ZERO             TO  UT1-IDDISTR                              
207400       MOVE ZERO             TO  UT1-IDKUNDNR                             
207500       MOVE ZERO             TO  UT1-IDFAKT                               
207600       MOVE SPACE            TO  UT1-KDFAKTYP                             
207700       MOVE ZERO             TO  UT1-DAFAKT                               
207800       MOVE ZERO             TO  UT1-IDORDNR7                             
207900       MOVE 1A17-IDARTNR     TO  UT1-IDARTNR                              
208000       MOVE 1A17-KDPRODSL    TO  UT1-KDPRODSL                             
208100       MOVE ZERO             TO  UT1-KDPSLLOC                             
208200       MOVE ZERO             TO  UT1-KVLEVART                             
208300       MOVE ZERO             TO  UT1-PRARTNTO                             
208400       MOVE SPACE            TO  UT1-FLOVRLEV                             
208500       MOVE ZERO             TO  UT1-KDFRAKT                              
208600       MOVE ZERO             TO  UT1-SUFAKTRE                             
208700       MOVE ZERO             TO  UT1-PREMBHNT                             
208800       MOVE ZERO             TO  UT1-PRFRAKT                              
208900       MOVE ZERO             TO  UT1-PRFOERS                              
209000       MOVE ZERO             TO  UT1-PRMOMS                               
209100       MOVE ZERO             TO  UT1-PRLEGKST                             
209200       MOVE ZERO             TO  UT1-SUFKTTILL                            
209300       MOVE ZERO             TO  UT1-PRAVDRAG                             
209400       MOVE ZERO             TO  UT1-SUFKTBEL                             
209500       MOVE ZERO             TO  UT1-SUFKTUTL                             
209600       MOVE ZERO             TO  UT1-PRKURS                               
209700       MOVE ZERO             TO  UT1-IDRAPPNR                             
209800       MOVE ZERO             TO  UT1-IDKOLLI                              
209900       MOVE SPACE            TO  UT1-KDANMORS                             
210000       MOVE SPACE            TO  UT1-KDVALISO                             
210100       MOVE ZERO             TO  UT1-DAINLINL                             
210200       MOVE ZERO             TO  UT1-KVANTMOT                             
210300       MOVE ZERO             TO  UT1-KVSKROT                              
210400       MOVE 1A17-PRAVCOST    TO  UT1-PRAVCOST                             
210500       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
210600       MOVE ZERO             TO  UT1-KVLS-OLD                             
210700       MOVE SPACE            TO  UT1-FLSLUT                               
210800       MOVE ZERO             TO  UT1-REMARKUP                             
210900       MOVE ZERO             TO  UT1-IDKNOTNR                             
211000       MOVE ZERO             TO  UT1-DAKRENOT                             
211100       MOVE ZERO             TO  UT1-SUKREUTL                             
211200       MOVE ZERO             TO  UT1-SUKRENTO                             
211300       MOVE ZERO             TO  UT1-PRLANDCO                             
211400       MOVE ZERO             TO  UT1-SUKRENOT                             
211500       MOVE ZERO             TO  UT1-KVKREANT                             
211600       MOVE ZERO             TO  UT1-DARETILL                             
211700       MOVE ZERO             TO  UT1-KVLEVANM                             
211800       MOVE ZERO             TO  UT1-KVRETINL                             
211900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
212000       MOVE ZERO             TO  UT1-IDKONTO                              
212100       MOVE SPACE            TO  UT1-IDKST                                
212200       MOVE 1A17-DAJUSTDA    TO  UT1-DAJUSTDA                             
212300       MOVE ZERO             TO  UT1-KVJUSTKV                             
212400       MOVE ZERO             TO  UT1-KDINVKAT                             
212500       MOVE SPACE            TO  UT1-TEINVANM                             
212600       MOVE ZERO             TO  UT1-IDPRODNR                             
212700       MOVE SPACE            TO  UT1-IDLEVNR                              
212800       MOVE ZERO             TO  UT1-KVBEART                              
212900       MOVE ZERO             TO  UT1-DAORDDAT                             
213000       MOVE ZERO             TO  UT1-PRARTBEU                             
213100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
213200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
213300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
213400       MOVE SPACE            TO  UT1-IDFS                                 
213500       MOVE ZERO             TO  UT1-KVAVIS                               
213600       MOVE ZERO             TO  UT1-IDBYTRAP                             
213700       MOVE ZERO             TO  UT1-KVRETUR                              
213800       MOVE ZERO             TO  UT1-SUAVCOST                             
213900       MOVE SPACE            TO  UT1-IDUSER                               
214000       MOVE SPACE            TO  UT1-KDAVCOST                             
214100       MOVE 1A17-KDPSLLOC-NEW TO UT1-KDPSLLOC-NEW                         
214200       MOVE 1A17-KDPSLLOC-OLD TO UT1-KDPSLLOC-OLD                         
214300       MOVE 1A17-KVEFRS      TO  UT1-KVEFRS                               
214400       MOVE 1A17-KVLS        TO  UT1-KVLS                                 
214500       MOVE ZERO             TO  UT1-DAREGDAT                             
214600       MOVE ZERO             TO  UT1-DASTADAT                             
214700       MOVE ZERO             TO  UT1-PRKURS-SU                            
214800       MOVE ZERO             TO  UT1-PRKURS-SC                            
214900       MOVE ZERO             TO  UT1-PRKURS-UC                            
215000       MOVE ZERO             TO  UT1-PRKURS-CU                            
215010       PERFORM S11-WRITE-W56022A                                          
215100     END-IF                                                               
215200     IF IN1-IDPTYP = 'A18'                                                
215300       MOVE 1A18-IDPTYP      TO  UT1-IDPTYP                               
215400       MOVE 1A18-KDEKOHT     TO  UT1-KDEKOHT                              
215500       MOVE 1A18-IDFTG       TO  UT1-IDFTG                                
215600       MOVE 1A18-IDDC-SEND   TO  UT1-IDDC-SEND                            
215700       MOVE 1A18-IDDC-REC    TO  UT1-IDDC-REC                             
215800       MOVE ZERO             TO  UT1-IDDISTR                              
215900       MOVE ZERO             TO  UT1-IDKUNDNR                             
216000       MOVE ZERO             TO  UT1-IDFAKT                               
216100       MOVE SPACE            TO  UT1-KDFAKTYP                             
216200       MOVE ZERO             TO  UT1-DAFAKT                               
216300       MOVE ZERO             TO  UT1-IDORDNR7                             
216400       MOVE 1A18-IDARTNR     TO  UT1-IDARTNR                              
216500       MOVE 1A18-KDPRODSL    TO  UT1-KDPRODSL                             
216600       MOVE 1A18-KDPSLLOC    TO  UT1-KDPSLLOC                             
216700       MOVE ZERO             TO  UT1-KVLEVART                             
216800       MOVE ZERO             TO  UT1-PRARTNTO                             
216900       MOVE SPACE            TO  UT1-FLOVRLEV                             
217000       MOVE ZERO             TO  UT1-KDFRAKT                              
217100       MOVE ZERO             TO  UT1-SUFAKTRE                             
217200       MOVE ZERO             TO  UT1-PREMBHNT                             
217300       MOVE ZERO             TO  UT1-PRFRAKT                              
217400       MOVE ZERO             TO  UT1-PRFOERS                              
217500       MOVE ZERO             TO  UT1-PRMOMS                               
217600       MOVE ZERO             TO  UT1-PRLEGKST                             
217700       MOVE ZERO             TO  UT1-SUFKTTILL                            
217800       MOVE ZERO             TO  UT1-PRAVDRAG                             
217900       MOVE ZERO             TO  UT1-SUFKTBEL                             
218000       MOVE ZERO             TO  UT1-SUFKTUTL                             
218100       MOVE ZERO             TO  UT1-PRKURS                               
218200       MOVE ZERO             TO  UT1-IDRAPPNR                             
218300       MOVE ZERO             TO  UT1-IDKOLLI                              
218400       MOVE SPACE            TO  UT1-KDANMORS                             
218500       MOVE SPACE            TO  UT1-KDVALISO                             
218600       MOVE 1A18-DAINLINL    TO  UT1-DAINLINL                             
218700       MOVE 1A18-KVANTMOT    TO  UT1-KVANTMOT                             
218800       MOVE ZERO             TO  UT1-KVSKROT                              
218900       MOVE 1A18-PRAVCOST    TO  UT1-PRAVCOST                             
219000       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
219100       MOVE ZERO             TO  UT1-KVLS-OLD                             
219200       MOVE SPACE            TO  UT1-FLSLUT                               
219300       MOVE ZERO             TO  UT1-REMARKUP                             
219400       MOVE ZERO             TO  UT1-IDKNOTNR                             
219500       MOVE ZERO             TO  UT1-DAKRENOT                             
219600       MOVE ZERO             TO  UT1-SUKREUTL                             
219700       MOVE ZERO             TO  UT1-SUKRENTO                             
219800       MOVE ZERO             TO  UT1-PRLANDCO                             
219900       MOVE ZERO             TO  UT1-SUKRENOT                             
220000       MOVE ZERO             TO  UT1-KVKREANT                             
220100       MOVE ZERO             TO  UT1-DARETILL                             
220200       MOVE ZERO             TO  UT1-KVLEVANM                             
220300       MOVE ZERO             TO  UT1-KVRETINL                             
220400       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
220500       MOVE 1A18-IDKONTO     TO  UT1-IDKONTO                              
220600       MOVE SPACE            TO  UT1-IDKST                                
220700       MOVE ZERO             TO  UT1-DAJUSTDA                             
220800       MOVE ZERO             TO  UT1-KVJUSTKV                             
220900       MOVE ZERO             TO  UT1-KDINVKAT                             
221000       MOVE SPACE            TO  UT1-TEINVANM                             
221100       MOVE ZERO             TO  UT1-IDPRODNR                             
221200       MOVE 1A18-IDLEVNR     TO  UT1-IDLEVNR                              
221300       MOVE ZERO             TO  UT1-KVBEART                              
221400       MOVE ZERO             TO  UT1-DAORDDAT                             
221500       MOVE ZERO             TO  UT1-PRARTBEU                             
221600       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
221700       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
221800       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
221900       MOVE 1A18-IDFS        TO  UT1-IDFS                                 
222000       MOVE ZERO             TO  UT1-KVAVIS                               
222100       MOVE ZERO             TO  UT1-IDBYTRAP                             
222200       MOVE ZERO             TO  UT1-KVRETUR                              
222300       MOVE ZERO             TO  UT1-SUAVCOST                             
222400       MOVE SPACE            TO  UT1-IDUSER                               
222500       MOVE SPACE            TO  UT1-KDAVCOST                             
222600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
222700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
222800       MOVE ZERO             TO  UT1-KVEFRS                               
222900       MOVE ZERO             TO  UT1-KVLS                                 
223000       MOVE ZERO             TO  UT1-DAREGDAT                             
223100       MOVE ZERO             TO  UT1-DASTADAT                             
223200       MOVE ZERO             TO  UT1-PRKURS-SU                            
223300       MOVE ZERO             TO  UT1-PRKURS-SC                            
223400       MOVE ZERO             TO  UT1-PRKURS-UC                            
223500       MOVE ZERO             TO  UT1-PRKURS-CU                            
223510       PERFORM S11-WRITE-W56022A                                          
223600     END-IF                                                               
223700     IF IN1-IDPTYP = 'A19'                                                
223800       MOVE 1A19-IDPTYP      TO  UT1-IDPTYP                               
223900       MOVE 1A19-KDEKOHT     TO  UT1-KDEKOHT                              
224000       MOVE 1A19-IDFTG       TO  UT1-IDFTG                                
224100       MOVE 1A19-IDDC-SEND   TO  UT1-IDDC-SEND                            
224200       MOVE 1A19-IDDC-REC    TO  UT1-IDDC-REC                             
224300       MOVE ZERO             TO  UT1-IDDISTR                              
224400       MOVE ZERO             TO  UT1-IDKUNDNR                             
224500       MOVE ZERO             TO  UT1-IDFAKT                               
224600       MOVE SPACE            TO  UT1-KDFAKTYP                             
224700       MOVE ZERO             TO  UT1-DAFAKT                               
224800       MOVE ZERO             TO  UT1-IDORDNR7                             
224900       MOVE 1A19-IDARTNR     TO  UT1-IDARTNR                              
225000       MOVE 1A19-KDPRODSL    TO  UT1-KDPRODSL                             
225100       MOVE 1A19-KDPSLLOC    TO  UT1-KDPSLLOC                             
225200       MOVE ZERO             TO  UT1-KVLEVART                             
225300       MOVE ZERO             TO  UT1-PRARTNTO                             
225400       MOVE SPACE            TO  UT1-FLOVRLEV                             
225500       MOVE ZERO             TO  UT1-KDFRAKT                              
225600       MOVE ZERO             TO  UT1-SUFAKTRE                             
225700       MOVE ZERO             TO  UT1-PREMBHNT                             
225800       MOVE ZERO             TO  UT1-PRFRAKT                              
225900       MOVE ZERO             TO  UT1-PRFOERS                              
226000       MOVE ZERO             TO  UT1-PRMOMS                               
226100       MOVE ZERO             TO  UT1-PRLEGKST                             
226200       MOVE ZERO             TO  UT1-SUFKTTILL                            
226300       MOVE ZERO             TO  UT1-PRAVDRAG                             
226400       MOVE ZERO             TO  UT1-SUFKTBEL                             
226500       MOVE ZERO             TO  UT1-SUFKTUTL                             
226600       MOVE ZERO             TO  UT1-PRKURS                               
226700       MOVE ZERO             TO  UT1-IDRAPPNR                             
226800       MOVE ZERO             TO  UT1-IDKOLLI                              
226900       MOVE SPACE            TO  UT1-KDANMORS                             
227000       MOVE SPACE            TO  UT1-KDVALISO                             
227100       MOVE ZERO             TO  UT1-DAINLINL                             
227200       MOVE ZERO             TO  UT1-KVANTMOT                             
227300       MOVE ZERO             TO  UT1-KVSKROT                              
227400       MOVE 1A19-PRAVCOST    TO  UT1-PRAVCOST                             
227500       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
227600       MOVE ZERO             TO  UT1-KVLS-OLD                             
227700       MOVE SPACE            TO  UT1-FLSLUT                               
227800       MOVE ZERO             TO  UT1-REMARKUP                             
227900       MOVE ZERO             TO  UT1-IDKNOTNR                             
228000       MOVE ZERO             TO  UT1-DAKRENOT                             
228100       MOVE ZERO             TO  UT1-SUKREUTL                             
228200       MOVE ZERO             TO  UT1-SUKRENTO                             
228300       MOVE ZERO             TO  UT1-PRLANDCO                             
228400       MOVE ZERO             TO  UT1-SUKRENOT                             
228500       MOVE ZERO             TO  UT1-KVKREANT                             
228600       MOVE ZERO             TO  UT1-DARETILL                             
228700       MOVE ZERO             TO  UT1-KVLEVANM                             
228800       MOVE ZERO             TO  UT1-KVRETINL                             
228900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
229000       MOVE ZERO             TO  UT1-IDKONTO                              
229100       MOVE SPACE            TO  UT1-IDKST                                
229200       MOVE ZERO             TO  UT1-DAJUSTDA                             
229300       MOVE 1A19-KVJUSTKV    TO  UT1-KVJUSTKV                             
229400       MOVE ZERO             TO  UT1-KDINVKAT                             
229500       MOVE SPACE            TO  UT1-TEINVANM                             
229600       MOVE ZERO             TO  UT1-IDPRODNR                             
229700       MOVE SPACE            TO  UT1-IDLEVNR                              
229800       MOVE ZERO             TO  UT1-KVBEART                              
229900       MOVE ZERO             TO  UT1-DAORDDAT                             
230000       MOVE ZERO             TO  UT1-PRARTBEU                             
230100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
230200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
230300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
230400       MOVE 1A19-IDFS        TO  UT1-IDFS                                 
230500       MOVE ZERO             TO  UT1-KVAVIS                               
230600       MOVE ZERO             TO  UT1-IDBYTRAP                             
230700       MOVE ZERO             TO  UT1-KVRETUR                              
230800       MOVE ZERO             TO  UT1-SUAVCOST                             
230900       MOVE SPACE            TO  UT1-IDUSER                               
231000       MOVE SPACE            TO  UT1-KDAVCOST                             
231100       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
231200       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
231300       MOVE ZERO             TO  UT1-KVEFRS                               
231400       MOVE ZERO             TO  UT1-KVLS                                 
231500       MOVE 1A19-DAREGDAT    TO  UT1-DAREGDAT                             
231600       MOVE ZERO             TO  UT1-DASTADAT                             
231700       MOVE ZERO             TO  UT1-PRKURS-SU                            
231800       MOVE ZERO             TO  UT1-PRKURS-SC                            
231900       MOVE ZERO             TO  UT1-PRKURS-UC                            
232000       MOVE ZERO             TO  UT1-PRKURS-CU                            
232010       PERFORM S11-WRITE-W56022A                                          
232100     END-IF                                                               
232200     IF IN1-IDPTYP = 'A20'                                                
232300       MOVE 1A20-IDPTYP      TO  UT1-IDPTYP                               
232400       MOVE 1A20-KDEKOHT     TO  UT1-KDEKOHT                              
232500       MOVE 1A20-IDFTG       TO  UT1-IDFTG                                
232600       MOVE 1A20-IDDC-SEND   TO  UT1-IDDC-SEND                            
232700       MOVE 1A20-IDDC-REC    TO  UT1-IDDC-REC                             
232800       MOVE ZERO             TO  UT1-IDDISTR                              
232900       MOVE ZERO             TO  UT1-IDKUNDNR                             
233000       MOVE ZERO             TO  UT1-IDFAKT                               
233100       MOVE SPACE            TO  UT1-KDFAKTYP                             
233200       MOVE ZERO             TO  UT1-DAFAKT                               
233300       MOVE ZERO             TO  UT1-IDORDNR7                             
233400       MOVE ZERO             TO  UT1-IDARTNR                              
233500       MOVE ZERO             TO  UT1-KDPRODSL                             
233600       MOVE ZERO             TO  UT1-KDPSLLOC                             
233700       MOVE ZERO             TO  UT1-KVLEVART                             
233800       MOVE ZERO             TO  UT1-PRARTNTO                             
233900       MOVE SPACE            TO  UT1-FLOVRLEV                             
234000       MOVE ZERO             TO  UT1-KDFRAKT                              
234100       MOVE ZERO             TO  UT1-SUFAKTRE                             
234200       MOVE ZERO             TO  UT1-PREMBHNT                             
234300       MOVE ZERO             TO  UT1-PRFRAKT                              
234400       MOVE ZERO             TO  UT1-PRFOERS                              
234500       MOVE ZERO             TO  UT1-PRMOMS                               
234600       MOVE ZERO             TO  UT1-PRLEGKST                             
234700       MOVE ZERO             TO  UT1-SUFKTTILL                            
234800       MOVE ZERO             TO  UT1-PRAVDRAG                             
234900       MOVE ZERO             TO  UT1-SUFKTBEL                             
235000       MOVE ZERO             TO  UT1-SUFKTUTL                             
235100       MOVE ZERO             TO  UT1-PRKURS                               
235200       MOVE ZERO             TO  UT1-IDRAPPNR                             
235300       MOVE ZERO             TO  UT1-IDKOLLI                              
235400       MOVE SPACE            TO  UT1-KDANMORS                             
235500       MOVE SPACE            TO  UT1-KDVALISO                             
235600       MOVE ZERO             TO  UT1-DAINLINL                             
235700       MOVE ZERO             TO  UT1-KVANTMOT                             
235800       MOVE ZERO             TO  UT1-KVSKROT                              
235900       MOVE ZERO             TO  UT1-PRAVCOST                             
236000       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
236100       MOVE ZERO             TO  UT1-KVLS-OLD                             
236200       MOVE SPACE            TO  UT1-FLSLUT                               
236300       MOVE ZERO             TO  UT1-REMARKUP                             
236400       MOVE ZERO             TO  UT1-IDKNOTNR                             
236500       MOVE ZERO             TO  UT1-DAKRENOT                             
236600       MOVE ZERO             TO  UT1-SUKREUTL                             
236700       MOVE ZERO             TO  UT1-SUKRENTO                             
236800       MOVE ZERO             TO  UT1-PRLANDCO                             
236900       MOVE ZERO             TO  UT1-SUKRENOT                             
237000       MOVE ZERO             TO  UT1-KVKREANT                             
237100       MOVE ZERO             TO  UT1-DARETILL                             
237200       MOVE ZERO             TO  UT1-KVLEVANM                             
237300       MOVE ZERO             TO  UT1-KVRETINL                             
237400       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
237500       MOVE ZERO             TO  UT1-IDKONTO                              
237600       MOVE SPACE            TO  UT1-IDKST                                
237700       MOVE ZERO             TO  UT1-DAJUSTDA                             
237800       MOVE ZERO             TO  UT1-KVJUSTKV                             
237900       MOVE ZERO             TO  UT1-KDINVKAT                             
238000       MOVE SPACE            TO  UT1-TEINVANM                             
238100       MOVE ZERO             TO  UT1-IDPRODNR                             
238200       MOVE SPACE            TO  UT1-IDLEVNR                              
238300       MOVE ZERO             TO  UT1-KVBEART                              
238400       MOVE ZERO             TO  UT1-DAORDDAT                             
238500       MOVE ZERO             TO  UT1-PRARTBEU                             
238600       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
238700       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
238800       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
238900       MOVE SPACE            TO  UT1-IDFS                                 
239000       MOVE ZERO             TO  UT1-KVAVIS                               
239100       MOVE ZERO             TO  UT1-IDBYTRAP                             
239200       MOVE ZERO             TO  UT1-KVRETUR                              
239300       MOVE ZERO             TO  UT1-SUAVCOST                             
239400       MOVE SPACE            TO  UT1-IDUSER                               
239500       MOVE SPACE            TO  UT1-KDAVCOST                             
239600       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
239700       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
239800       MOVE ZERO             TO  UT1-KVEFRS                               
239900       MOVE ZERO             TO  UT1-KVLS                                 
240000       MOVE ZERO             TO  UT1-DAREGDAT                             
240100       MOVE 1A20-DASTADAT    TO  UT1-DASTADAT                             
240200       MOVE 1A20-PRKURS-SU   TO  UT1-PRKURS-SU                            
240300       MOVE 1A20-PRKURS-SC   TO  UT1-PRKURS-SC                            
240400       MOVE 1A20-PRKURS-UC   TO  UT1-PRKURS-UC                            
240500       MOVE 1A20-PRKURS-CU   TO  UT1-PRKURS-CU                            
240510       PERFORM S11-WRITE-W56022A                                          
240600     END-IF                                                               
240700     IF IN1-IDPTYP = 'L09'                                                
240800       MOVE 1L09-IDPTYP      TO  UT1-IDPTYP                               
240900       MOVE 1L09-KDEKOHT     TO  UT1-KDEKOHT                              
241000       MOVE 1L09-IDFTG       TO  UT1-IDFTG                                
241100       MOVE 1L09-IDDC-SEND   TO  UT1-IDDC-SEND                            
241200       MOVE 1L09-IDDC-REC    TO  UT1-IDDC-REC                             
241300       MOVE 1L09-IDDISTR     TO  UT1-IDDISTR                              
241400       MOVE 1L09-IDKUNDNR    TO  UT1-IDKUNDNR                             
241500       MOVE 1L09-IDFAKT      TO  UT1-IDFAKT                               
241600       MOVE SPACE            TO  UT1-KDFAKTYP                             
241700       MOVE 1L09-DAFAKT      TO  UT1-DAFAKT                               
241800       MOVE 1L09-IDORDNR7    TO  UT1-IDORDNR7                             
241900       MOVE 1L09-IDARTNR     TO  UT1-IDARTNR                              
242000       MOVE 1L09-KDPRODSL    TO  UT1-KDPRODSL                             
242100       MOVE 1L09-KDPSLLOC    TO  UT1-KDPSLLOC                             
242200       MOVE 1L09-KVLEVART    TO  UT1-KVLEVART                             
242300       MOVE ZERO             TO  UT1-PRARTNTO                             
242400       MOVE 1L09-FLOVRLEV    TO  UT1-FLOVRLEV                             
242500       MOVE ZERO             TO  UT1-KDFRAKT                              
242600       MOVE ZERO             TO  UT1-SUFAKTRE                             
242700       MOVE ZERO             TO  UT1-PREMBHNT                             
242800       MOVE ZERO             TO  UT1-PRFRAKT                              
242900       MOVE ZERO             TO  UT1-PRFOERS                              
243000       MOVE ZERO             TO  UT1-PRMOMS                               
243100       MOVE ZERO             TO  UT1-PRLEGKST                             
243200       MOVE ZERO             TO  UT1-SUFKTTILL                            
243300       MOVE ZERO             TO  UT1-PRAVDRAG                             
243400       MOVE ZERO             TO  UT1-SUFKTBEL                             
243500       MOVE ZERO             TO  UT1-SUFKTUTL                             
243600       MOVE ZERO             TO  UT1-PRKURS                               
243700       MOVE ZERO             TO  UT1-IDRAPPNR                             
243800       MOVE ZERO             TO  UT1-IDKOLLI                              
243900       MOVE SPACE            TO  UT1-KDANMORS                             
244000       MOVE SPACE            TO  UT1-KDVALISO                             
244100       MOVE ZERO             TO  UT1-DAINLINL                             
244200       MOVE ZERO             TO  UT1-KVANTMOT                             
244300       MOVE ZERO             TO  UT1-KVSKROT                              
244400       MOVE 1L09-PRAVCOST    TO  UT1-PRAVCOST                             
244500       MOVE ZERO             TO  UT1-PRAVCOST-OLD                         
244600       MOVE ZERO             TO  UT1-KVLS-OLD                             
244700       MOVE SPACE            TO  UT1-FLSLUT                               
244800       MOVE ZERO             TO  UT1-REMARKUP                             
244900       MOVE ZERO             TO  UT1-IDKNOTNR                             
245000       MOVE ZERO             TO  UT1-DAKRENOT                             
245100       MOVE ZERO             TO  UT1-SUKREUTL                             
245200       MOVE ZERO             TO  UT1-SUKRENTO                             
245300       MOVE ZERO             TO  UT1-PRLANDCO                             
245400       MOVE ZERO             TO  UT1-SUKRENOT                             
245500       MOVE ZERO             TO  UT1-KVKREANT                             
245600       MOVE ZERO             TO  UT1-DARETILL                             
245700       MOVE ZERO             TO  UT1-KVLEVANM                             
245800       MOVE ZERO             TO  UT1-KVRETINL                             
245900       MOVE ZERO             TO  UT1-KVRETINL-SKR                         
246000       MOVE ZERO             TO  UT1-IDKONTO                              
246100       MOVE SPACE            TO  UT1-IDKST                                
246200       MOVE ZERO             TO  UT1-DAJUSTDA                             
246300       MOVE ZERO             TO  UT1-KVJUSTKV                             
246400       MOVE ZERO             TO  UT1-KDINVKAT                             
246500       MOVE SPACE            TO  UT1-TEINVANM                             
246600       MOVE ZERO             TO  UT1-IDPRODNR                             
246700       MOVE SPACE            TO  UT1-IDLEVNR                              
246800       MOVE ZERO             TO  UT1-KVBEART                              
246900       MOVE ZERO             TO  UT1-DAORDDAT                             
247000       MOVE ZERO             TO  UT1-PRARTBEU                             
247100       MOVE ZERO             TO  UT1-PRARTNTO-GNB                         
247200       MOVE SPACE            TO  UT1-IDFAKT-GNB                           
247300       MOVE ZERO             TO  UT1-DAFAKT-GNB                           
247400       MOVE SPACE            TO  UT1-IDFS                                 
247500       MOVE ZERO             TO  UT1-KVAVIS                               
247600       MOVE ZERO             TO  UT1-IDBYTRAP                             
247700       MOVE ZERO             TO  UT1-KVRETUR                              
247800       MOVE ZERO             TO  UT1-SUAVCOST                             
247900       MOVE SPACE            TO  UT1-IDUSER                               
248000       MOVE SPACE            TO  UT1-KDAVCOST                             
248100       MOVE ZERO             TO  UT1-KDPSLLOC-NEW                         
248200       MOVE ZERO             TO  UT1-KDPSLLOC-OLD                         
248300       MOVE ZERO             TO  UT1-KVEFRS                               
248400       MOVE ZERO             TO  UT1-KVLS                                 
248500       MOVE ZERO             TO  UT1-DAREGDAT                             
248600       MOVE ZERO             TO  UT1-DASTADAT                             
248700       MOVE ZERO             TO  UT1-PRKURS-SU                            
248800       MOVE ZERO             TO  UT1-PRKURS-SC                            
248900       MOVE ZERO             TO  UT1-PRKURS-UC                            
249000       MOVE ZERO             TO  UT1-PRKURS-CU                            
249010       PERFORM S11-WRITE-W56022A                                          
249100     END-IF                                                               
249110     MOVE SPACE              TO UT1-HEADER                                
249200     .                                                                    
249300     EJECT                                                                
249400 C-MOVE-DATA SECTION.                                                     
249500     IF IN2-IDPTYP = 'AX2'                                                
249600       MOVE 2AX2-IDPTYP      TO  UT2-IDPTYP                               
249700       MOVE 2AX2-KDEKOHT     TO  UT2-KDEKOHT                              
249800       MOVE 2AX2-IDFTG       TO  UT2-IDFTG                                
249900       MOVE 2AX2-IDDC-SEND   TO  UT2-IDDC-SEND                            
250000       MOVE 2AX2-IDDC-REC    TO  UT2-IDDC-REC                             
250100       MOVE 2AX2-IDDISTR     TO  UT2-IDDISTR                              
250200       MOVE 2AX2-IDKUNDNR    TO  UT2-IDKUNDNR                             
250300       MOVE 2AX2-IDFAKT      TO  UT2-IDFAKT                               
250400       MOVE 2AX2-KDFAKTYP    TO  UT2-KDFAKTYP                             
250500       MOVE 2AX2-DAFAKT      TO  UT2-DAFAKT                               
250600       MOVE 2AX2-IDORDNR7    TO  UT2-IDORDNR7                             
250700       MOVE 2AX2-IDARTNR     TO  UT2-IDARTNR                              
250800       MOVE 2AX2-KDPRODSL    TO  UT2-KDPRODSL                             
250900       MOVE 2AX2-KDPSLLOC    TO  UT2-KDPSLLOC                             
251000       MOVE 2AX2-KVLEVART    TO  UT2-KVLEVART                             
251100       MOVE 2AX2-PRARTNTO    TO  UT2-PRARTNTO                             
251200       MOVE 2AX2-FLOVRLEV    TO  UT2-FLOVRLEV                             
251300       MOVE ZERO             TO  UT2-KDFRAKT                              
251400       MOVE ZERO             TO  UT2-SUFAKTRE                             
251500       MOVE ZERO             TO  UT2-PREMBHNT                             
251600       MOVE ZERO             TO  UT2-PRFRAKT                              
251700       MOVE ZERO             TO  UT2-PRFOERS                              
251800       MOVE ZERO             TO  UT2-PRMOMS                               
251900       MOVE ZERO             TO  UT2-PRLEGKST                             
252000       MOVE ZERO             TO  UT2-SUFKTTILL                            
252100       MOVE ZERO             TO  UT2-PRAVDRAG                             
252200       MOVE ZERO             TO  UT2-SUFKTBEL                             
252300       MOVE ZERO             TO  UT2-SUFKTUTL                             
252400       MOVE ZERO             TO  UT2-PRKURS                               
252500       MOVE ZERO             TO  UT2-IDRAPPNR                             
252600       MOVE ZERO             TO  UT2-IDKOLLI                              
252700       MOVE SPACE            TO  UT2-KDANMORS                             
252800       MOVE SPACE            TO  UT2-KDVALISO                             
252900       MOVE ZERO             TO  UT2-DAINLINL                             
253000       MOVE ZERO             TO  UT2-KVANTMOT                             
253100       MOVE ZERO             TO  UT2-KVSKROT                              
253200       MOVE ZERO             TO  UT2-PRAVCOST                             
253300       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
253400       MOVE ZERO             TO  UT2-KVLS-OLD                             
253500       MOVE SPACE            TO  UT2-FLSLUT                               
253600       MOVE ZERO             TO  UT2-REMARKUP                             
253700       MOVE ZERO             TO  UT2-IDKNOTNR                             
253800       MOVE ZERO             TO  UT2-DAKRENOT                             
253900       MOVE ZERO             TO  UT2-SUKREUTL                             
254000       MOVE ZERO             TO  UT2-SUKRENTO                             
254100       MOVE ZERO             TO  UT2-PRLANDCO                             
254200       MOVE ZERO             TO  UT2-SUKRENOT                             
254300       MOVE ZERO             TO  UT2-KVKREANT                             
254400       MOVE ZERO             TO  UT2-DARETILL                             
254500       MOVE ZERO             TO  UT2-KVLEVANM                             
254600       MOVE ZERO             TO  UT2-KVRETINL                             
254700       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
254800       MOVE ZERO             TO  UT2-IDKONTO                              
254900       MOVE SPACE            TO  UT2-IDKST                                
255000       MOVE ZERO             TO  UT2-DAJUSTDA                             
255100       MOVE ZERO             TO  UT2-KVJUSTKV                             
255200       MOVE ZERO             TO  UT2-KDINVKAT                             
255300       MOVE SPACE            TO  UT2-TEINVANM                             
255400       MOVE ZERO             TO  UT2-IDPRODNR                             
255500       MOVE SPACE            TO  UT2-IDLEVNR                              
255600       MOVE ZERO             TO  UT2-KVBEART                              
255700       MOVE ZERO             TO  UT2-DAORDDAT                             
255800       MOVE ZERO             TO  UT2-PRARTBEU                             
255900       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
256000       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
256100       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
256200       MOVE SPACE            TO  UT2-IDFS                                 
256300       MOVE ZERO             TO  UT2-KVAVIS                               
256400       MOVE ZERO             TO  UT2-IDBYTRAP                             
256500       MOVE ZERO             TO  UT2-KVRETUR                              
256600       MOVE ZERO             TO  UT2-SUAVCOST                             
256700       MOVE SPACE            TO  UT2-IDUSER                               
256800       MOVE SPACE            TO  UT2-KDAVCOST                             
256900       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
257000       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
257100       MOVE ZERO             TO  UT2-KVEFRS                               
257200       MOVE ZERO             TO  UT2-KVLS                                 
257300       MOVE ZERO             TO  UT2-DAREGDAT                             
257400       MOVE ZERO             TO  UT2-DASTADAT                             
257500       MOVE ZERO             TO  UT2-PRKURS-SU                            
257600       MOVE ZERO             TO  UT2-PRKURS-SC                            
257700       MOVE ZERO             TO  UT2-PRKURS-UC                            
257800       MOVE ZERO             TO  UT2-PRKURS-CU                            
257810       PERFORM S12-WRITE-W56023A                                          
257900     END-IF                                                               
258000     IF IN2-IDPTYP = 'A01'                                                
258100       MOVE 2A01-IDPTYP      TO  UT2-IDPTYP                               
258200       MOVE 2A01-KDEKOHT     TO  UT2-KDEKOHT                              
258300       MOVE 2A01-IDFTG       TO  UT2-IDFTG                                
258400       MOVE 2A01-IDDC-SEND   TO  UT2-IDDC-SEND                            
258500       MOVE 2A01-IDDC-REC    TO  UT2-IDDC-REC                             
258600       MOVE 2A01-IDDISTR     TO  UT2-IDDISTR                              
258700       MOVE 2A01-IDKUNDNR    TO  UT2-IDKUNDNR                             
258800       MOVE 2A01-IDFAKT      TO  UT2-IDFAKT                               
258900       MOVE 2A01-KDFAKTYP    TO  UT2-KDFAKTYP                             
259000       MOVE 2A01-DAFAKT      TO  UT2-DAFAKT                               
259100       MOVE ZERO             TO  UT2-IDORDNR7                             
259200       MOVE ZERO             TO  UT2-IDARTNR                              
259300       MOVE ZERO             TO  UT2-KDPRODSL                             
259400       MOVE ZERO             TO  UT2-KDPSLLOC                             
259500       MOVE ZERO             TO  UT2-KVLEVART                             
259600       MOVE ZERO             TO  UT2-PRARTNTO                             
259700       MOVE SPACE            TO  UT2-FLOVRLEV                             
259800       MOVE ZERO             TO  UT2-KDFRAKT                              
259900       MOVE 2A01-SUFAKTRE    TO  UT2-SUFAKTRE                             
260000       MOVE 2A01-PREMBHNT    TO  UT2-PREMBHNT                             
260100       MOVE 2A01-PRFRAKT     TO  UT2-PRFRAKT                              
260200       MOVE 2A01-PRFOERS     TO  UT2-PRFOERS                              
260300       MOVE 2A01-PRMOMS      TO  UT2-PRMOMS                               
260400       MOVE 2A01-PRLEGKST    TO  UT2-PRLEGKST                             
260500       MOVE 2A01-SUFKTTILL   TO  UT2-SUFKTTILL                            
260600       MOVE 2A01-PRAVDRAG    TO  UT2-PRAVDRAG                             
260700       MOVE 2A01-SUFKTBEL    TO  UT2-SUFKTBEL                             
260800       MOVE 2A01-SUFKTUTL    TO  UT2-SUFKTUTL                             
260900       MOVE 2A01-PRKURS      TO  UT2-PRKURS                               
261000       MOVE 2A01-IDRAPPNR    TO  UT2-IDRAPPNR                             
261100       MOVE ZERO             TO  UT2-IDKOLLI                              
261200       MOVE SPACE            TO  UT2-KDANMORS                             
261300       MOVE SPACE            TO  UT2-KDVALISO                             
261400       MOVE ZERO             TO  UT2-DAINLINL                             
261500       MOVE ZERO             TO  UT2-KVANTMOT                             
261600       MOVE ZERO             TO  UT2-KVSKROT                              
261700       MOVE ZERO             TO  UT2-PRAVCOST                             
261800       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
261900       MOVE ZERO             TO  UT2-KVLS-OLD                             
262000       MOVE SPACE            TO  UT2-FLSLUT                               
262100       MOVE ZERO             TO  UT2-REMARKUP                             
262200       MOVE ZERO             TO  UT2-IDKNOTNR                             
262300       MOVE ZERO             TO  UT2-DAKRENOT                             
262400       MOVE ZERO             TO  UT2-SUKREUTL                             
262500       MOVE ZERO             TO  UT2-SUKRENTO                             
262600       MOVE ZERO             TO  UT2-PRLANDCO                             
262700       MOVE ZERO             TO  UT2-SUKRENOT                             
262800       MOVE ZERO             TO  UT2-KVKREANT                             
262900       MOVE ZERO             TO  UT2-DARETILL                             
263000       MOVE ZERO             TO  UT2-KVLEVANM                             
263100       MOVE ZERO             TO  UT2-KVRETINL                             
263200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
263300       MOVE ZERO             TO  UT2-IDKONTO                              
263400       MOVE SPACE            TO  UT2-IDKST                                
263500       MOVE ZERO             TO  UT2-DAJUSTDA                             
263600       MOVE ZERO             TO  UT2-KVJUSTKV                             
263700       MOVE ZERO             TO  UT2-KDINVKAT                             
263800       MOVE SPACE            TO  UT2-TEINVANM                             
263900       MOVE ZERO             TO  UT2-IDPRODNR                             
264000       MOVE SPACE            TO  UT2-IDLEVNR                              
264100       MOVE ZERO             TO  UT2-KVBEART                              
264200       MOVE ZERO             TO  UT2-DAORDDAT                             
264300       MOVE ZERO             TO  UT2-PRARTBEU                             
264400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
264500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
264600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
264700       MOVE SPACE            TO  UT2-IDFS                                 
264800       MOVE ZERO             TO  UT2-KVAVIS                               
264900       MOVE ZERO             TO  UT2-IDBYTRAP                             
265000       MOVE ZERO             TO  UT2-KVRETUR                              
265100       MOVE ZERO             TO  UT2-SUAVCOST                             
265200       MOVE SPACE            TO  UT2-IDUSER                               
265300       MOVE SPACE            TO  UT2-KDAVCOST                             
265400       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
265500       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
265600       MOVE ZERO             TO  UT2-KVEFRS                               
265700       MOVE ZERO             TO  UT2-KVLS                                 
265800       MOVE ZERO             TO  UT2-DAREGDAT                             
265900       MOVE ZERO             TO  UT2-DASTADAT                             
266000       MOVE ZERO             TO  UT2-PRKURS-SU                            
266100       MOVE ZERO             TO  UT2-PRKURS-SC                            
266200       MOVE ZERO             TO  UT2-PRKURS-UC                            
266300       MOVE ZERO             TO  UT2-PRKURS-CU                            
266310       PERFORM S12-WRITE-W56023A                                          
266400     END-IF                                                               
266500     IF IN2-IDPTYP = 'A02'                                                
266600       MOVE 2A02-IDPTYP      TO  UT2-IDPTYP                               
266700       MOVE 2A02-KDEKOHT     TO  UT2-KDEKOHT                              
266800       MOVE 2A02-IDFTG       TO  UT2-IDFTG                                
266900       MOVE 2A02-IDDC-SEND   TO  UT2-IDDC-SEND                            
267000       MOVE 2A02-IDDC-REC    TO  UT2-IDDC-REC                             
267100       MOVE 2A02-IDDISTR     TO  UT2-IDDISTR                              
267200       MOVE 2A02-IDKUNDNR    TO  UT2-IDKUNDNR                             
267300       MOVE 2A02-IDFAKT      TO  UT2-IDFAKT                               
267400       MOVE 2A02-KDFAKTYP    TO  UT2-KDFAKTYP                             
267500       MOVE 2A02-DAFAKT      TO  UT2-DAFAKT                               
267600       MOVE 2A02-IDORDNR7    TO  UT2-IDORDNR7                             
267700       MOVE 2A02-IDARTNR     TO  UT2-IDARTNR                              
267800       MOVE 2A02-KDPRODSL    TO  UT2-KDPRODSL                             
267900       MOVE 2A02-KDPSLLOC    TO  UT2-KDPSLLOC                             
268000       MOVE 2A02-KVLEVART    TO  UT2-KVLEVART                             
268100       MOVE 2A02-PRARTNTO    TO  UT2-PRARTNTO                             
268200       MOVE SPACE            TO  UT2-FLOVRLEV                             
268300       MOVE ZERO             TO  UT2-KDFRAKT                              
268400       MOVE ZERO             TO  UT2-SUFAKTRE                             
268500       MOVE ZERO             TO  UT2-PREMBHNT                             
268600       MOVE ZERO             TO  UT2-PRFRAKT                              
268700       MOVE ZERO             TO  UT2-PRFOERS                              
268800       MOVE ZERO             TO  UT2-PRMOMS                               
268900       MOVE ZERO             TO  UT2-PRLEGKST                             
269000       MOVE ZERO             TO  UT2-SUFKTTILL                            
269100       MOVE ZERO             TO  UT2-PRAVDRAG                             
269200       MOVE ZERO             TO  UT2-SUFKTBEL                             
269300       MOVE ZERO             TO  UT2-SUFKTUTL                             
269400       MOVE ZERO             TO  UT2-PRKURS                               
269500       MOVE ZERO             TO  UT2-IDRAPPNR                             
269600       MOVE ZERO             TO  UT2-IDKOLLI                              
269700       MOVE SPACE            TO  UT2-KDANMORS                             
269800       MOVE SPACE            TO  UT2-KDVALISO                             
269900       MOVE ZERO             TO  UT2-DAINLINL                             
270000       MOVE ZERO             TO  UT2-KVANTMOT                             
270100       MOVE ZERO             TO  UT2-KVSKROT                              
270200       MOVE ZERO             TO  UT2-PRAVCOST                             
270300       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
270400       MOVE ZERO             TO  UT2-KVLS-OLD                             
270500       MOVE SPACE            TO  UT2-FLSLUT                               
270600       MOVE ZERO             TO  UT2-REMARKUP                             
270700       MOVE ZERO             TO  UT2-IDKNOTNR                             
270800       MOVE ZERO             TO  UT2-DAKRENOT                             
270900       MOVE ZERO             TO  UT2-SUKREUTL                             
271000       MOVE ZERO             TO  UT2-SUKRENTO                             
271100       MOVE ZERO             TO  UT2-PRLANDCO                             
271200       MOVE ZERO             TO  UT2-SUKRENOT                             
271300       MOVE ZERO             TO  UT2-KVKREANT                             
271400       MOVE ZERO             TO  UT2-DARETILL                             
271500       MOVE ZERO             TO  UT2-KVLEVANM                             
271600       MOVE ZERO             TO  UT2-KVRETINL                             
271700       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
271800       MOVE ZERO             TO  UT2-IDKONTO                              
271900       MOVE SPACE            TO  UT2-IDKST                                
272000       MOVE ZERO             TO  UT2-DAJUSTDA                             
272100       MOVE ZERO             TO  UT2-KVJUSTKV                             
272200       MOVE ZERO             TO  UT2-KDINVKAT                             
272300       MOVE SPACE            TO  UT2-TEINVANM                             
272400       MOVE ZERO             TO  UT2-IDPRODNR                             
272500       MOVE SPACE            TO  UT2-IDLEVNR                              
272600       MOVE ZERO             TO  UT2-KVBEART                              
272700       MOVE ZERO             TO  UT2-DAORDDAT                             
272800       MOVE ZERO             TO  UT2-PRARTBEU                             
272900       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
273000       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
273100       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
273200       MOVE SPACE            TO  UT2-IDFS                                 
273300       MOVE ZERO             TO  UT2-KVAVIS                               
273400       MOVE ZERO             TO  UT2-IDBYTRAP                             
273500       MOVE ZERO             TO  UT2-KVRETUR                              
273600       MOVE ZERO             TO  UT2-SUAVCOST                             
273700       MOVE SPACE            TO  UT2-IDUSER                               
273800       MOVE SPACE            TO  UT2-KDAVCOST                             
273900       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
274000       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
274100       MOVE ZERO             TO  UT2-KVEFRS                               
274200       MOVE ZERO             TO  UT2-KVLS                                 
274300       MOVE ZERO             TO  UT2-DAREGDAT                             
274400       MOVE ZERO             TO  UT2-DASTADAT                             
274500       MOVE ZERO             TO  UT2-PRKURS-SU                            
274600       MOVE ZERO             TO  UT2-PRKURS-SC                            
274700       MOVE ZERO             TO  UT2-PRKURS-UC                            
274800       MOVE ZERO             TO  UT2-PRKURS-CU                            
274810       PERFORM S12-WRITE-W56023A                                          
274900     END-IF                                                               
275000     IF IN2-IDPTYP = 'A03'                                                
275100       MOVE 2A03-IDPTYP      TO  UT2-IDPTYP                               
275200       MOVE 2A03-KDEKOHT     TO  UT2-KDEKOHT                              
275300       MOVE 2A03-IDFTG       TO  UT2-IDFTG                                
275400       MOVE 2A03-IDDC-SEND   TO  UT2-IDDC-SEND                            
275500       MOVE 2A03-IDDC-REC    TO  UT2-IDDC-REC                             
275600       MOVE 2A03-IDDISTR     TO  UT2-IDDISTR                              
275700       MOVE 2A03-IDKUNDNR    TO  UT2-IDKUNDNR                             
275800       MOVE 2A03-IDFAKT      TO  UT2-IDFAKT                               
275900       MOVE space            TO  UT2-KDFAKTYP                             
276000       MOVE 2A03-DAFAKT      TO  UT2-DAFAKT                               
276100       MOVE 2A03-IDORDNR7    TO  UT2-IDORDNR7                             
276200       MOVE 2A03-IDARTNR     TO  UT2-IDARTNR                              
276300       MOVE 2A03-KDPRODSL    TO  UT2-KDPRODSL                             
276400       MOVE 2A03-KDPSLLOC    TO  UT2-KDPSLLOC                             
276500       MOVE 2A03-KVLEVART    TO  UT2-KVLEVART                             
276600       MOVE 2A03-PRARTNTO    TO  UT2-PRARTNTO                             
276700       MOVE SPACE            TO  UT2-FLOVRLEV                             
276800       MOVE ZERO             TO  UT2-KDFRAKT                              
276900       MOVE ZERO             TO  UT2-SUFAKTRE                             
277000       MOVE ZERO             TO  UT2-PREMBHNT                             
277100       MOVE ZERO             TO  UT2-PRFRAKT                              
277200       MOVE ZERO             TO  UT2-PRFOERS                              
277300       MOVE ZERO             TO  UT2-PRMOMS                               
277400       MOVE ZERO             TO  UT2-PRLEGKST                             
277500       MOVE ZERO             TO  UT2-SUFKTTILL                            
277600       MOVE ZERO             TO  UT2-PRAVDRAG                             
277700       MOVE ZERO             TO  UT2-SUFKTBEL                             
277800       MOVE ZERO             TO  UT2-SUFKTUTL                             
277900       MOVE ZERO             TO  UT2-PRKURS                               
278000       MOVE ZERO             TO  UT2-IDRAPPNR                             
278100       MOVE 2A03-IDKOLLI     TO  UT2-IDKOLLI                              
278200       MOVE 2A03-KDANMORS    TO  UT2-KDANMORS                             
278300       MOVE 2A03-KDVALISO    TO  UT2-KDVALISO                             
278400       MOVE 2A03-DAINLINL    TO  UT2-DAINLINL                             
278500       MOVE 2A03-KVANTMOT    TO  UT2-KVANTMOT                             
278600       MOVE 2A03-KVSKROT     TO  UT2-KVSKROT                              
278700       MOVE 2A03-PRAVCOST    TO  UT2-PRAVCOST                             
278800       MOVE 2A03-PRAVCOST-OLD TO UT2-PRAVCOST-OLD                         
278900       MOVE ZERO             TO  UT2-KVLS-OLD                             
279000       MOVE SPACE            TO  UT2-FLSLUT                               
279100       MOVE ZERO             TO  UT2-REMARKUP                             
279200       MOVE ZERO             TO  UT2-IDKNOTNR                             
279300       MOVE ZERO             TO  UT2-DAKRENOT                             
279400       MOVE ZERO             TO  UT2-SUKREUTL                             
279500       MOVE ZERO             TO  UT2-SUKRENTO                             
279600       MOVE ZERO             TO  UT2-PRLANDCO                             
279700       MOVE ZERO             TO  UT2-SUKRENOT                             
279800       MOVE ZERO             TO  UT2-KVKREANT                             
279900       MOVE ZERO             TO  UT2-DARETILL                             
280000       MOVE ZERO             TO  UT2-KVLEVANM                             
280100       MOVE ZERO             TO  UT2-KVRETINL                             
280200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
280300       MOVE ZERO             TO  UT2-IDKONTO                              
280400       MOVE SPACE            TO  UT2-IDKST                                
280500       MOVE ZERO             TO  UT2-DAJUSTDA                             
280600       MOVE ZERO             TO  UT2-KVJUSTKV                             
280700       MOVE ZERO             TO  UT2-KDINVKAT                             
280800       MOVE SPACE            TO  UT2-TEINVANM                             
280900       MOVE ZERO             TO  UT2-IDPRODNR                             
281000       MOVE SPACE            TO  UT2-IDLEVNR                              
281100       MOVE ZERO             TO  UT2-KVBEART                              
281200       MOVE ZERO             TO  UT2-DAORDDAT                             
281300       MOVE ZERO             TO  UT2-PRARTBEU                             
281400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
281500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
281600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
281700       MOVE SPACE            TO  UT2-IDFS                                 
281800       MOVE ZERO             TO  UT2-KVAVIS                               
281900       MOVE ZERO             TO  UT2-IDBYTRAP                             
282000       MOVE ZERO             TO  UT2-KVRETUR                              
282100       MOVE ZERO             TO  UT2-SUAVCOST                             
282200       MOVE SPACE            TO  UT2-IDUSER                               
282300       MOVE SPACE            TO  UT2-KDAVCOST                             
282400       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
282500       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
282600       MOVE ZERO             TO  UT2-KVEFRS                               
282700       MOVE ZERO             TO  UT2-KVLS                                 
282800       MOVE ZERO             TO  UT2-DAREGDAT                             
282900       MOVE ZERO             TO  UT2-DASTADAT                             
283000       MOVE ZERO             TO  UT2-PRKURS-SU                            
283100       MOVE ZERO             TO  UT2-PRKURS-SC                            
283200       MOVE ZERO             TO  UT2-PRKURS-UC                            
283300       MOVE ZERO             TO  UT2-PRKURS-CU                            
283310       PERFORM S12-WRITE-W56023A                                          
283400     END-IF                                                               
283500     IF IN2-IDPTYP = 'A04'                                                
283600       MOVE 2A04-IDPTYP      TO  UT2-IDPTYP                               
283700       MOVE 2A04-KDEKOHT     TO  UT2-KDEKOHT                              
283800       MOVE 2A04-IDFTG       TO  UT2-IDFTG                                
283900       MOVE 2A04-IDDC-SEND   TO  UT2-IDDC-SEND                            
284000       MOVE 2A04-IDDC-REC    TO  UT2-IDDC-REC                             
284100       MOVE 2A04-IDDISTR     TO  UT2-IDDISTR                              
284200       MOVE 2A04-IDKUNDNR    TO  UT2-IDKUNDNR                             
284300       MOVE ZERO             TO  UT2-IDFAKT                               
284400       MOVE SPACE            TO  UT2-KDFAKTYP                             
284500       MOVE ZERO             TO  UT2-DAFAKT                               
284600       MOVE ZERO             TO  UT2-IDORDNR7                             
284700       MOVE ZERO             TO  UT2-IDARTNR                              
284800       MOVE ZERO             TO  UT2-KDPRODSL                             
284900       MOVE ZERO             TO  UT2-KDPSLLOC                             
285000       MOVE ZERO             TO  UT2-KVLEVART                             
285100       MOVE ZERO             TO  UT2-PRARTNTO                             
285200       MOVE SPACE            TO  UT2-FLOVRLEV                             
285300       MOVE ZERO             TO  UT2-KDFRAKT                              
285400       MOVE ZERO             TO  UT2-SUFAKTRE                             
285500       MOVE 2A04-PREMBHNT    TO  UT2-PREMBHNT                             
285600       MOVE 2A04-PRFRAKT     TO  UT2-PRFRAKT                              
285700       MOVE 2A04-PRFOERS     TO  UT2-PRFOERS                              
285800       MOVE 2A04-PRMOMS      TO  UT2-PRMOMS                               
285900       MOVE 2A04-PRLEGKST    TO  UT2-PRLEGKST                             
286000       MOVE ZERO             TO  UT2-SUFKTTILL                            
286100       MOVE ZERO             TO  UT2-PRAVDRAG                             
286200       MOVE ZERO             TO  UT2-SUFKTBEL                             
286300       MOVE ZERO             TO  UT2-SUFKTUTL                             
286400       MOVE 2A04-PRKURS      TO  UT2-PRKURS                               
286500       MOVE 2A04-IDRAPPNR    TO  UT2-IDRAPPNR                             
286600       MOVE ZERO             TO  UT2-IDKOLLI                              
286700       MOVE SPACE            TO  UT2-KDANMORS                             
286800       MOVE SPACE            TO  UT2-KDVALISO                             
286900       MOVE ZERO             TO  UT2-DAINLINL                             
287000       MOVE ZERO             TO  UT2-KVANTMOT                             
287100       MOVE ZERO             TO  UT2-KVSKROT                              
287200       MOVE ZERO             TO  UT2-PRAVCOST                             
287300       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
287400       MOVE ZERO             TO  UT2-KVLS-OLD                             
287500       MOVE SPACE            TO  UT2-FLSLUT                               
287600       MOVE ZERO             TO  UT2-REMARKUP                             
287700       MOVE 2A04-IDKNOTNR    TO  UT2-IDKNOTNR                             
287800       MOVE 2A04-DAKRENOT    TO  UT2-DAKRENOT                             
287900       MOVE 2A04-SUKREUTL    TO  UT2-SUKREUTL                             
288000       MOVE 2A04-SUKRENTO    TO  UT2-SUKRENTO                             
288100       MOVE 2A04-PRLANDCO    TO  UT2-PRLANDCO                             
288200       MOVE 2A04-SUKRENOT    TO  UT2-SUKRENOT                             
288300       MOVE ZERO             TO  UT2-KVKREANT                             
288400       MOVE ZERO             TO  UT2-DARETILL                             
288500       MOVE ZERO             TO  UT2-KVLEVANM                             
288600       MOVE ZERO             TO  UT2-KVRETINL                             
288700       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
288800       MOVE ZERO             TO  UT2-IDKONTO                              
288900       MOVE SPACE            TO  UT2-IDKST                                
289000       MOVE ZERO             TO  UT2-DAJUSTDA                             
289100       MOVE ZERO             TO  UT2-KVJUSTKV                             
289200       MOVE ZERO             TO  UT2-KDINVKAT                             
289300       MOVE SPACE            TO  UT2-TEINVANM                             
289400       MOVE ZERO             TO  UT2-IDPRODNR                             
289500       MOVE SPACE            TO  UT2-IDLEVNR                              
289600       MOVE ZERO             TO  UT2-KVBEART                              
289700       MOVE ZERO             TO  UT2-DAORDDAT                             
289800       MOVE ZERO             TO  UT2-PRARTBEU                             
289900       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
290000       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
290100       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
290200       MOVE SPACE            TO  UT2-IDFS                                 
290300       MOVE ZERO             TO  UT2-KVAVIS                               
290400       MOVE ZERO             TO  UT2-IDBYTRAP                             
290500       MOVE ZERO             TO  UT2-KVRETUR                              
290600       MOVE ZERO             TO  UT2-SUAVCOST                             
290700       MOVE SPACE            TO  UT2-IDUSER                               
290800       MOVE SPACE            TO  UT2-KDAVCOST                             
290900       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
291000       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
291100       MOVE ZERO             TO  UT2-KVEFRS                               
291200       MOVE ZERO             TO  UT2-KVLS                                 
291300       MOVE ZERO             TO  UT2-DAREGDAT                             
291400       MOVE ZERO             TO  UT2-DASTADAT                             
291500       MOVE ZERO             TO  UT2-PRKURS-SU                            
291600       MOVE ZERO             TO  UT2-PRKURS-SC                            
291700       MOVE ZERO             TO  UT2-PRKURS-UC                            
291800       MOVE ZERO             TO  UT2-PRKURS-CU                            
291810       PERFORM S12-WRITE-W56023A                                          
291900     END-IF                                                               
292000     IF IN2-IDPTYP = 'A05'                                                
292100       MOVE 2A05-IDPTYP      TO  UT2-IDPTYP                               
292200       MOVE 2A05-KDEKOHT     TO  UT2-KDEKOHT                              
292300       MOVE 2A05-IDFTG       TO  UT2-IDFTG                                
292400       MOVE 2A05-IDDC-SEND   TO  UT2-IDDC-SEND                            
292500       MOVE 2A05-IDDC-REC    TO  UT2-IDDC-REC                             
292600       MOVE 2A05-IDDISTR     TO  UT2-IDDISTR                              
292700       MOVE 2A05-IDKUNDNR    TO  UT2-IDKUNDNR                             
292800       MOVE ZERO             TO  UT2-IDFAKT                               
292900       MOVE SPACE            TO  UT2-KDFAKTYP                             
293000       MOVE ZERO             TO  UT2-DAFAKT                               
293100       MOVE ZERO             TO  UT2-IDORDNR7                             
293200       MOVE 2A05-IDARTNR     TO  UT2-IDARTNR                              
293300       MOVE 2A05-KDPRODSL    TO  UT2-KDPRODSL                             
293400       MOVE 2A05-KDPSLLOC    TO  UT2-KDPSLLOC                             
293500       MOVE ZERO             TO  UT2-KVLEVART                             
293600       MOVE 2A05-PRARTNTO    TO  UT2-PRARTNTO                             
293700       MOVE SPACE            TO  UT2-FLOVRLEV                             
293800       MOVE ZERO             TO  UT2-KDFRAKT                              
293900       MOVE ZERO             TO  UT2-SUFAKTRE                             
294000       MOVE ZERO             TO  UT2-PREMBHNT                             
294100       MOVE ZERO             TO  UT2-PRFRAKT                              
294200       MOVE ZERO             TO  UT2-PRFOERS                              
294300       MOVE ZERO             TO  UT2-PRMOMS                               
294400       MOVE ZERO             TO  UT2-PRLEGKST                             
294500       MOVE ZERO             TO  UT2-SUFKTTILL                            
294600       MOVE ZERO             TO  UT2-PRAVDRAG                             
294700       MOVE ZERO             TO  UT2-SUFKTBEL                             
294800       MOVE ZERO             TO  UT2-SUFKTUTL                             
294900       MOVE ZERO             TO  UT2-PRKURS                               
295000       MOVE ZERO             TO  UT2-IDRAPPNR                             
295100       MOVE ZERO             TO  UT2-IDKOLLI                              
295200       MOVE 2A05-KDANMORS    TO  UT2-KDANMORS                             
295300       MOVE SPACE            TO  UT2-KDVALISO                             
295400       MOVE ZERO             TO  UT2-DAINLINL                             
295500       MOVE ZERO             TO  UT2-KVANTMOT                             
295600       MOVE ZERO             TO  UT2-KVSKROT                              
295700       MOVE ZERO             TO  UT2-PRAVCOST                             
295800       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
295900       MOVE ZERO             TO  UT2-KVLS-OLD                             
296000       MOVE SPACE            TO  UT2-FLSLUT                               
296100       MOVE ZERO             TO  UT2-REMARKUP                             
296200       MOVE 2A05-IDKNOTNR    TO  UT2-IDKNOTNR                             
296300       MOVE 2A05-DAKRENOT    TO  UT2-DAKRENOT                             
296400       MOVE ZERO             TO  UT2-SUKREUTL                             
296500       MOVE ZERO             TO  UT2-SUKRENTO                             
296600       MOVE ZERO             TO  UT2-PRLANDCO                             
296700       MOVE ZERO             TO  UT2-SUKRENOT                             
296800       MOVE 2A05-KVKREANT    TO  UT2-KVKREANT                             
296900       MOVE ZERO             TO  UT2-DARETILL                             
297000       MOVE ZERO             TO  UT2-KVLEVANM                             
297100       MOVE ZERO             TO  UT2-KVRETINL                             
297200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
297300       MOVE ZERO             TO  UT2-IDKONTO                              
297400       MOVE SPACE            TO  UT2-IDKST                                
297500       MOVE ZERO             TO  UT2-DAJUSTDA                             
297600       MOVE ZERO             TO  UT2-KVJUSTKV                             
297700       MOVE ZERO             TO  UT2-KDINVKAT                             
297800       MOVE SPACE            TO  UT2-TEINVANM                             
297900       MOVE ZERO             TO  UT2-IDPRODNR                             
298000       MOVE SPACE            TO  UT2-IDLEVNR                              
298100       MOVE ZERO             TO  UT2-KVBEART                              
298200       MOVE ZERO             TO  UT2-DAORDDAT                             
298300       MOVE ZERO             TO  UT2-PRARTBEU                             
298400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
298500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
298600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
298700       MOVE SPACE            TO  UT2-IDFS                                 
298800       MOVE ZERO             TO  UT2-KVAVIS                               
298900       MOVE ZERO             TO  UT2-IDBYTRAP                             
299000       MOVE ZERO             TO  UT2-KVRETUR                              
299100       MOVE ZERO             TO  UT2-SUAVCOST                             
299200       MOVE SPACE            TO  UT2-IDUSER                               
299300       MOVE SPACE            TO  UT2-KDAVCOST                             
299400       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
299500       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
299600       MOVE ZERO             TO  UT2-KVEFRS                               
299700       MOVE ZERO             TO  UT2-KVLS                                 
299800       MOVE ZERO             TO  UT2-DAREGDAT                             
299900       MOVE ZERO             TO  UT2-DASTADAT                             
300000       MOVE ZERO             TO  UT2-PRKURS-SU                            
300100       MOVE ZERO             TO  UT2-PRKURS-SC                            
300200       MOVE ZERO             TO  UT2-PRKURS-UC                            
300300       MOVE ZERO             TO  UT2-PRKURS-CU                            
300310       PERFORM S12-WRITE-W56023A                                          
300400     END-IF                                                               
300500     IF IN2-IDPTYP = 'A06'                                                
300600       MOVE 2A06-IDPTYP      TO  UT2-IDPTYP                               
300700       MOVE 2A06-KDEKOHT     TO  UT2-KDEKOHT                              
300800       MOVE 2A06-IDFTG       TO  UT2-IDFTG                                
300900       MOVE 2A06-IDDC-SEND   TO  UT2-IDDC-SEND                            
301000       MOVE 2A06-IDDC-REC    TO  UT2-IDDC-REC                             
301100       MOVE 2A06-IDDISTR     TO  UT2-IDDISTR                              
301200       MOVE 2A06-IDKUNDNR    TO  UT2-IDKUNDNR                             
301300       MOVE ZERO             TO  UT2-IDFAKT                               
301400       MOVE SPACE            TO  UT2-KDFAKTYP                             
301500       MOVE ZERO             TO  UT2-DAFAKT                               
301600       MOVE ZERO             TO  UT2-IDORDNR7                             
301700       MOVE 2A06-IDARTNR     TO  UT2-IDARTNR                              
301800       MOVE 2A06-KDPRODSL    TO  UT2-KDPRODSL                             
301900       MOVE 2A06-KDPSLLOC    TO  UT2-KDPSLLOC                             
302000       MOVE ZERO             TO  UT2-KVLEVART                             
302100       MOVE ZERO             TO  UT2-PRARTNTO                             
302200       MOVE SPACE            TO  UT2-FLOVRLEV                             
302300       MOVE ZERO             TO  UT2-KDFRAKT                              
302400       MOVE ZERO             TO  UT2-SUFAKTRE                             
302500       MOVE ZERO             TO  UT2-PREMBHNT                             
302600       MOVE ZERO             TO  UT2-PRFRAKT                              
302700       MOVE ZERO             TO  UT2-PRFOERS                              
302800       MOVE ZERO             TO  UT2-PRMOMS                               
302900       MOVE ZERO             TO  UT2-PRLEGKST                             
303000       MOVE ZERO             TO  UT2-SUFKTTILL                            
303100       MOVE ZERO             TO  UT2-PRAVDRAG                             
303200       MOVE ZERO             TO  UT2-SUFKTBEL                             
303300       MOVE ZERO             TO  UT2-SUFKTUTL                             
303400       MOVE ZERO             TO  UT2-PRKURS                               
303500       MOVE 2A06-IDRAPPNR    TO  UT2-IDRAPPNR                             
303600       MOVE ZERO             TO  UT2-IDKOLLI                              
303700       MOVE 2A06-KDANMORS    TO  UT2-KDANMORS                             
303800       MOVE SPACE            TO  UT2-KDVALISO                             
303900       MOVE ZERO             TO  UT2-DAINLINL                             
304000       MOVE ZERO             TO  UT2-KVANTMOT                             
304100       MOVE ZERO             TO  UT2-KVSKROT                              
304200       MOVE 2A06-PRAVCOST    TO  UT2-PRAVCOST                             
304300       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
304400       MOVE ZERO             TO  UT2-KVLS-OLD                             
304500       MOVE SPACE            TO  UT2-FLSLUT                               
304600       MOVE ZERO             TO  UT2-REMARKUP                             
304700       MOVE ZERO             TO  UT2-IDKNOTNR                             
304800       MOVE ZERO             TO  UT2-DAKRENOT                             
304900       MOVE ZERO             TO  UT2-SUKREUTL                             
305000       MOVE ZERO             TO  UT2-SUKRENTO                             
305100       MOVE ZERO             TO  UT2-PRLANDCO                             
305200       MOVE ZERO             TO  UT2-SUKRENOT                             
305300       MOVE ZERO             TO  UT2-KVKREANT                             
305400       MOVE 2A06-DARETILL    TO  UT2-DARETILL                             
305500       MOVE 2A06-KVLEVANM    TO  UT2-KVLEVANM                             
305600       MOVE 2A06-KVRETINL    TO  UT2-KVRETINL                             
305700       MOVE 2A06-KVRETINL-SKR TO UT2-KVRETINL-SKR                         
305800       MOVE ZERO             TO  UT2-IDKONTO                              
305900       MOVE SPACE            TO  UT2-IDKST                                
306000       MOVE ZERO             TO  UT2-DAJUSTDA                             
306100       MOVE ZERO             TO  UT2-KVJUSTKV                             
306200       MOVE ZERO             TO  UT2-KDINVKAT                             
306300       MOVE SPACE            TO  UT2-TEINVANM                             
306400       MOVE ZERO             TO  UT2-IDPRODNR                             
306500       MOVE SPACE            TO  UT2-IDLEVNR                              
306600       MOVE ZERO             TO  UT2-KVBEART                              
306700       MOVE ZERO             TO  UT2-DAORDDAT                             
306800       MOVE ZERO             TO  UT2-PRARTBEU                             
306900       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
307000       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
307100       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
307200       MOVE SPACE            TO  UT2-IDFS                                 
307300       MOVE ZERO             TO  UT2-KVAVIS                               
307400       MOVE ZERO             TO  UT2-IDBYTRAP                             
307500       MOVE ZERO             TO  UT2-KVRETUR                              
307600       MOVE ZERO             TO  UT2-SUAVCOST                             
307700       MOVE SPACE            TO  UT2-IDUSER                               
307800       MOVE SPACE            TO  UT2-KDAVCOST                             
307900       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
308000       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
308100       MOVE ZERO             TO  UT2-KVEFRS                               
308200       MOVE ZERO             TO  UT2-KVLS                                 
308300       MOVE ZERO             TO  UT2-DAREGDAT                             
308400       MOVE ZERO             TO  UT2-DASTADAT                             
308500       MOVE ZERO             TO  UT2-PRKURS-SU                            
308600       MOVE ZERO             TO  UT2-PRKURS-SC                            
308700       MOVE ZERO             TO  UT2-PRKURS-UC                            
308800       MOVE ZERO             TO  UT2-PRKURS-CU                            
308810       PERFORM S12-WRITE-W56023A                                          
308900     END-IF                                                               
309000     IF IN2-IDPTYP = 'A07'                                                
309100       MOVE 2A07-IDPTYP      TO  UT2-IDPTYP                               
309200       MOVE 2A07-KDEKOHT     TO  UT2-KDEKOHT                              
309300       MOVE 2A07-IDFTG       TO  UT2-IDFTG                                
309400       MOVE 2A07-IDDC-SEND   TO  UT2-IDDC-SEND                            
309500       MOVE 2A07-IDDC-REC    TO  UT2-IDDC-REC                             
309600       MOVE 2A07-IDDISTR     TO  UT2-IDDISTR                              
309700       MOVE 2A07-IDKUNDNR    TO  UT2-IDKUNDNR                             
309800       MOVE 2A07-IDFAKT      TO  UT2-IDFAKT                               
309900       MOVE SPACE            TO  UT2-KDFAKTYP                             
310000       MOVE 2A07-DAFAKT      TO  UT2-DAFAKT                               
310100       MOVE 2A07-IDORDNR7    TO  UT2-IDORDNR7                             
310200       MOVE 2A07-IDARTNR     TO  UT2-IDARTNR                              
310300       MOVE 2A07-KDPRODSL    TO  UT2-KDPRODSL                             
310400       MOVE 2A07-KDPSLLOC    TO  UT2-KDPSLLOC                             
310500       MOVE 2A07-KVLEVART    TO  UT2-KVLEVART                             
310600       MOVE ZERO             TO  UT2-PRARTNTO                             
310700       MOVE SPACE            TO  UT2-FLOVRLEV                             
310800       MOVE ZERO             TO  UT2-KDFRAKT                              
310900       MOVE ZERO             TO  UT2-SUFAKTRE                             
311000       MOVE ZERO             TO  UT2-PREMBHNT                             
311100       MOVE ZERO             TO  UT2-PRFRAKT                              
311200       MOVE ZERO             TO  UT2-PRFOERS                              
311300       MOVE ZERO             TO  UT2-PRMOMS                               
311400       MOVE ZERO             TO  UT2-PRLEGKST                             
311500       MOVE ZERO             TO  UT2-SUFKTTILL                            
311600       MOVE ZERO             TO  UT2-PRAVDRAG                             
311700       MOVE ZERO             TO  UT2-SUFKTBEL                             
311800       MOVE ZERO             TO  UT2-SUFKTUTL                             
311900       MOVE ZERO             TO  UT2-PRKURS                               
312000       MOVE ZERO             TO  UT2-IDRAPPNR                             
312100       MOVE ZERO             TO  UT2-IDKOLLI                              
312200       MOVE SPACE            TO  UT2-KDANMORS                             
312300       MOVE SPACE            TO  UT2-KDVALISO                             
312400       MOVE ZERO             TO  UT2-DAINLINL                             
312500       MOVE ZERO             TO  UT2-KVANTMOT                             
312600       MOVE ZERO             TO  UT2-KVSKROT                              
312700       MOVE 2A07-PRAVCOST    TO  UT2-PRAVCOST                             
312800       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
312900       MOVE ZERO             TO  UT2-KVLS-OLD                             
313000       MOVE SPACE            TO  UT2-FLSLUT                               
313100       MOVE ZERO             TO  UT2-REMARKUP                             
313200       MOVE ZERO             TO  UT2-IDKNOTNR                             
313300       MOVE ZERO             TO  UT2-DAKRENOT                             
313400       MOVE ZERO             TO  UT2-SUKREUTL                             
313500       MOVE ZERO             TO  UT2-SUKRENTO                             
313600       MOVE ZERO             TO  UT2-PRLANDCO                             
313700       MOVE ZERO             TO  UT2-SUKRENOT                             
313800       MOVE ZERO             TO  UT2-KVKREANT                             
313900       MOVE ZERO             TO  UT2-DARETILL                             
314000       MOVE ZERO             TO  UT2-KVLEVANM                             
314100       MOVE ZERO             TO  UT2-KVRETINL                             
314200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
314300       MOVE 2A07-IDKONTO     TO  UT2-IDKONTO                              
314400       MOVE 2A07-IDKST       TO  UT2-IDKST                                
314500       MOVE ZERO             TO  UT2-DAJUSTDA                             
314600       MOVE ZERO             TO  UT2-KVJUSTKV                             
314700       MOVE ZERO             TO  UT2-KDINVKAT                             
314800       MOVE SPACE            TO  UT2-TEINVANM                             
314900       MOVE ZERO             TO  UT2-IDPRODNR                             
315000       MOVE SPACE            TO  UT2-IDLEVNR                              
315100       MOVE ZERO             TO  UT2-KVBEART                              
315200       MOVE ZERO             TO  UT2-DAORDDAT                             
315300       MOVE ZERO             TO  UT2-PRARTBEU                             
315400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
315500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
315600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
315700       MOVE SPACE            TO  UT2-IDFS                                 
315800       MOVE ZERO             TO  UT2-KVAVIS                               
315900       MOVE ZERO             TO  UT2-IDBYTRAP                             
316000       MOVE ZERO             TO  UT2-KVRETUR                              
316100       MOVE ZERO             TO  UT2-SUAVCOST                             
316200       MOVE SPACE            TO  UT2-IDUSER                               
316300       MOVE SPACE            TO  UT2-KDAVCOST                             
316400       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
316500       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
316600       MOVE ZERO             TO  UT2-KVEFRS                               
316700       MOVE ZERO             TO  UT2-KVLS                                 
316800       MOVE ZERO             TO  UT2-DAREGDAT                             
316900       MOVE ZERO             TO  UT2-DASTADAT                             
317000       MOVE ZERO             TO  UT2-PRKURS-SU                            
317100       MOVE ZERO             TO  UT2-PRKURS-SC                            
317200       MOVE ZERO             TO  UT2-PRKURS-UC                            
317300       MOVE ZERO             TO  UT2-PRKURS-CU                            
317310       PERFORM S12-WRITE-W56023A                                          
317400     END-IF                                                               
317500     IF IN2-IDPTYP = 'A08'                                                
317600       MOVE 2A08-IDPTYP      TO  UT2-IDPTYP                               
317700       MOVE 2A08-KDEKOHT     TO  UT2-KDEKOHT                              
317800       MOVE 2A08-IDFTG       TO  UT2-IDFTG                                
317900       MOVE 2A08-IDDC-SEND   TO  UT2-IDDC-SEND                            
318000       MOVE 2A08-IDDC-REC    TO  UT2-IDDC-REC                             
318100       MOVE ZERO             TO  UT2-IDDISTR                              
318200       MOVE ZERO             TO  UT2-IDKUNDNR                             
318300       MOVE ZERO             TO  UT2-IDFAKT                               
318400       MOVE SPACE            TO  UT2-KDFAKTYP                             
318500       MOVE ZERO             TO  UT2-DAFAKT                               
318600       MOVE ZERO             TO  UT2-IDORDNR7                             
318700       MOVE 2A08-IDARTNR     TO  UT2-IDARTNR                              
318800       MOVE 2A08-KDPRODSL    TO  UT2-KDPRODSL                             
318900       MOVE 2A08-KDPSLLOC    TO  UT2-KDPSLLOC                             
319000       MOVE ZERO             TO  UT2-KVLEVART                             
319100       MOVE ZERO             TO  UT2-PRARTNTO                             
319200       MOVE SPACE            TO  UT2-FLOVRLEV                             
319300       MOVE ZERO             TO  UT2-KDFRAKT                              
319400       MOVE ZERO             TO  UT2-SUFAKTRE                             
319500       MOVE ZERO             TO  UT2-PREMBHNT                             
319600       MOVE ZERO             TO  UT2-PRFRAKT                              
319700       MOVE ZERO             TO  UT2-PRFOERS                              
319800       MOVE ZERO             TO  UT2-PRMOMS                               
319900       MOVE ZERO             TO  UT2-PRLEGKST                             
320000       MOVE ZERO             TO  UT2-SUFKTTILL                            
320100       MOVE ZERO             TO  UT2-PRAVDRAG                             
320200       MOVE ZERO             TO  UT2-SUFKTBEL                             
320300       MOVE ZERO             TO  UT2-SUFKTUTL                             
320400       MOVE ZERO             TO  UT2-PRKURS                               
320500       MOVE ZERO             TO  UT2-IDRAPPNR                             
320600       MOVE ZERO             TO  UT2-IDKOLLI                              
320700       MOVE SPACE            TO  UT2-KDANMORS                             
320800       MOVE SPACE            TO  UT2-KDVALISO                             
320900       MOVE ZERO             TO  UT2-DAINLINL                             
321000       MOVE ZERO             TO  UT2-KVANTMOT                             
321100       MOVE ZERO             TO  UT2-KVSKROT                              
321200       MOVE 2A08-PRAVCOST    TO  UT2-PRAVCOST                             
321300       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
321400       MOVE ZERO             TO  UT2-KVLS-OLD                             
321500       MOVE SPACE            TO  UT2-FLSLUT                               
321600       MOVE ZERO             TO  UT2-REMARKUP                             
321700       MOVE ZERO             TO  UT2-IDKNOTNR                             
321800       MOVE ZERO             TO  UT2-DAKRENOT                             
321900       MOVE ZERO             TO  UT2-SUKREUTL                             
322000       MOVE ZERO             TO  UT2-SUKRENTO                             
322100       MOVE ZERO             TO  UT2-PRLANDCO                             
322200       MOVE ZERO             TO  UT2-SUKRENOT                             
322300       MOVE ZERO             TO  UT2-KVKREANT                             
322400       MOVE ZERO             TO  UT2-DARETILL                             
322500       MOVE ZERO             TO  UT2-KVLEVANM                             
322600       MOVE ZERO             TO  UT2-KVRETINL                             
322700       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
322800       MOVE ZERO             TO  UT2-IDKONTO                              
322900       MOVE SPACE            TO  UT2-IDKST                                
323000       MOVE 2A08-DAJUSTDA    TO  UT2-DAJUSTDA                             
323100       MOVE 2A08-KVJUSTKV    TO  UT2-KVJUSTKV                             
323200       MOVE 2A08-KDINVKAT    TO  UT2-KDINVKAT                             
323300       MOVE 2A08-TEINVANM    TO  UT2-TEINVANM                             
323400       MOVE ZERO             TO  UT2-IDPRODNR                             
323500       MOVE SPACE            TO  UT2-IDLEVNR                              
323600       MOVE ZERO             TO  UT2-KVBEART                              
323700       MOVE ZERO             TO  UT2-DAORDDAT                             
323800       MOVE ZERO             TO  UT2-PRARTBEU                             
323900       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
324000       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
324100       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
324200       MOVE SPACE            TO  UT2-IDFS                                 
324300       MOVE ZERO             TO  UT2-KVAVIS                               
324400       MOVE ZERO             TO  UT2-IDBYTRAP                             
324500       MOVE ZERO             TO  UT2-KVRETUR                              
324600       MOVE ZERO             TO  UT2-SUAVCOST                             
324700       MOVE SPACE            TO  UT2-IDUSER                               
324800       MOVE SPACE            TO  UT2-KDAVCOST                             
324900       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
325000       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
325100       MOVE ZERO             TO  UT2-KVEFRS                               
325200       MOVE ZERO             TO  UT2-KVLS                                 
325300       MOVE ZERO             TO  UT2-DAREGDAT                             
325400       MOVE ZERO             TO  UT2-DASTADAT                             
325500       MOVE ZERO             TO  UT2-PRKURS-SU                            
325600       MOVE ZERO             TO  UT2-PRKURS-SC                            
325700       MOVE ZERO             TO  UT2-PRKURS-UC                            
325800       MOVE ZERO             TO  UT2-PRKURS-CU                            
325810       PERFORM S12-WRITE-W56023A                                          
325900     END-IF                                                               
326000     IF IN2-IDPTYP = 'A09'                                                
326100       MOVE 2A09-IDPTYP      TO  UT2-IDPTYP                               
326200       MOVE 2A09-KDEKOHT     TO  UT2-KDEKOHT                              
326300       MOVE 2A09-IDFTG       TO  UT2-IDFTG                                
326400       MOVE 2A09-IDDC-SEND   TO  UT2-IDDC-SEND                            
326500       MOVE 2A09-IDDC-REC    TO  UT2-IDDC-REC                             
326600       MOVE 2A09-IDDISTR     TO  UT2-IDDISTR                              
326700       MOVE 2A09-IDKUNDNR    TO  UT2-IDKUNDNR                             
326800       MOVE 2A09-IDFAKT      TO  UT2-IDFAKT                               
326900       MOVE SPACE            TO  UT2-KDFAKTYP                             
327000       MOVE 2A09-DAFAKT      TO  UT2-DAFAKT                               
327100       MOVE 2A09-IDORDNR7    TO  UT2-IDORDNR7                             
327200       MOVE 2A09-IDARTNR     TO  UT2-IDARTNR                              
327300       MOVE 2A09-KDPRODSL    TO  UT2-KDPRODSL                             
327400       MOVE 2A09-KDPSLLOC    TO  UT2-KDPSLLOC                             
327500       MOVE 2A09-KVLEVART    TO  UT2-KVLEVART                             
327600       MOVE ZERO             TO  UT2-PRARTNTO                             
327700       MOVE SPACE            TO  UT2-FLOVRLEV                             
327800       MOVE ZERO             TO  UT2-KDFRAKT                              
327900       MOVE ZERO             TO  UT2-SUFAKTRE                             
328000       MOVE ZERO             TO  UT2-PREMBHNT                             
328100       MOVE ZERO             TO  UT2-PRFRAKT                              
328200       MOVE ZERO             TO  UT2-PRFOERS                              
328300       MOVE ZERO             TO  UT2-PRMOMS                               
328400       MOVE ZERO             TO  UT2-PRLEGKST                             
328500       MOVE ZERO             TO  UT2-SUFKTTILL                            
328600       MOVE ZERO             TO  UT2-PRAVDRAG                             
328700       MOVE ZERO             TO  UT2-SUFKTBEL                             
328800       MOVE ZERO             TO  UT2-SUFKTUTL                             
328900       MOVE ZERO             TO  UT2-PRKURS                               
329000       MOVE ZERO             TO  UT2-IDRAPPNR                             
329100       MOVE ZERO             TO  UT2-IDKOLLI                              
329200       MOVE SPACE            TO  UT2-KDANMORS                             
329300       MOVE SPACE            TO  UT2-KDVALISO                             
329400       MOVE ZERO             TO  UT2-DAINLINL                             
329500       MOVE ZERO             TO  UT2-KVANTMOT                             
329600       MOVE ZERO             TO  UT2-KVSKROT                              
329700       MOVE 2A09-PRAVCOST    TO  UT2-PRAVCOST                             
329800       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
329900       MOVE ZERO             TO  UT2-KVLS-OLD                             
330000       MOVE SPACE            TO  UT2-FLSLUT                               
330100       MOVE ZERO             TO  UT2-REMARKUP                             
330200       MOVE ZERO             TO  UT2-IDKNOTNR                             
330300       MOVE ZERO             TO  UT2-DAKRENOT                             
330400       MOVE ZERO             TO  UT2-SUKREUTL                             
330500       MOVE ZERO             TO  UT2-SUKRENTO                             
330600       MOVE ZERO             TO  UT2-PRLANDCO                             
330700       MOVE ZERO             TO  UT2-SUKRENOT                             
330800       MOVE ZERO             TO  UT2-KVKREANT                             
330900       MOVE ZERO             TO  UT2-DARETILL                             
331000       MOVE ZERO             TO  UT2-KVLEVANM                             
331100       MOVE ZERO             TO  UT2-KVRETINL                             
331200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
331300       MOVE ZERO             TO  UT2-IDKONTO                              
331400       MOVE SPACE            TO  UT2-IDKST                                
331500       MOVE ZERO             TO  UT2-DAJUSTDA                             
331600       MOVE ZERO             TO  UT2-KVJUSTKV                             
331700       MOVE ZERO             TO  UT2-KDINVKAT                             
331800       MOVE SPACE            TO  UT2-TEINVANM                             
331900       MOVE ZERO             TO  UT2-IDPRODNR                             
332000       MOVE SPACE            TO  UT2-IDLEVNR                              
332100       MOVE ZERO             TO  UT2-KVBEART                              
332200       MOVE ZERO             TO  UT2-DAORDDAT                             
332300       MOVE ZERO             TO  UT2-PRARTBEU                             
332400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
332500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
332600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
332700       MOVE SPACE            TO  UT2-IDFS                                 
332800       MOVE ZERO             TO  UT2-KVAVIS                               
332900       MOVE ZERO             TO  UT2-IDBYTRAP                             
333000       MOVE ZERO             TO  UT2-KVRETUR                              
333100       MOVE ZERO             TO  UT2-SUAVCOST                             
333200       MOVE SPACE            TO  UT2-IDUSER                               
333300       MOVE SPACE            TO  UT2-KDAVCOST                             
333400       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
333500       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
333600       MOVE ZERO             TO  UT2-KVEFRS                               
333700       MOVE ZERO             TO  UT2-KVLS                                 
333800       MOVE ZERO             TO  UT2-DAREGDAT                             
333900       MOVE ZERO             TO  UT2-DASTADAT                             
334000       MOVE ZERO             TO  UT2-PRKURS-SU                            
334100       MOVE ZERO             TO  UT2-PRKURS-SC                            
334200       MOVE ZERO             TO  UT2-PRKURS-UC                            
334300       MOVE ZERO             TO  UT2-PRKURS-CU                            
334310       PERFORM S12-WRITE-W56023A                                          
334400     END-IF                                                               
334500     IF IN2-IDPTYP = 'A10'                                                
334600       MOVE 2A10-IDPTYP      TO  UT2-IDPTYP                               
334700       MOVE 2A10-KDEKOHT     TO  UT2-KDEKOHT                              
334800       MOVE 2A10-IDFTG       TO  UT2-IDFTG                                
334900       MOVE 2A10-IDDC-SEND   TO  UT2-IDDC-SEND                            
335000       MOVE 2A10-IDDC-REC    TO  UT2-IDDC-REC                             
335100       MOVE 2A10-IDDISTR     TO  UT2-IDDISTR                              
335200       MOVE 2A10-IDKUNDNR    TO  UT2-IDKUNDNR                             
335300       MOVE ZERO             TO  UT2-IDFAKT                               
335400       MOVE SPACE            TO  UT2-KDFAKTYP                             
335500       MOVE ZERO             TO  UT2-DAFAKT                               
335600       MOVE 2A10-IDORDNR7    TO  UT2-IDORDNR7                             
335700       MOVE 2A10-IDARTNR     TO  UT2-IDARTNR                              
335800       MOVE 2A10-KDPRODSL    TO  UT2-KDPRODSL                             
335900       MOVE 2A10-KDPSLLOC    TO  UT2-KDPSLLOC                             
336000       MOVE 2A10-KVLEVART    TO  UT2-KVLEVART                             
336100       MOVE ZERO             TO  UT2-PRARTNTO                             
336200       MOVE SPACE            TO  UT2-FLOVRLEV                             
336300       MOVE ZERO             TO  UT2-KDFRAKT                              
336400       MOVE ZERO             TO  UT2-SUFAKTRE                             
336500       MOVE ZERO             TO  UT2-PREMBHNT                             
336600       MOVE ZERO             TO  UT2-PRFRAKT                              
336700       MOVE ZERO             TO  UT2-PRFOERS                              
336800       MOVE ZERO             TO  UT2-PRMOMS                               
336900       MOVE ZERO             TO  UT2-PRLEGKST                             
337000       MOVE ZERO             TO  UT2-SUFKTTILL                            
337100       MOVE ZERO             TO  UT2-PRAVDRAG                             
337200       MOVE ZERO             TO  UT2-SUFKTBEL                             
337300       MOVE ZERO             TO  UT2-SUFKTUTL                             
337400       MOVE ZERO             TO  UT2-PRKURS                               
337500       MOVE ZERO             TO  UT2-IDRAPPNR                             
337600       MOVE ZERO             TO  UT2-IDKOLLI                              
337700       MOVE SPACE            TO  UT2-KDANMORS                             
337800       MOVE SPACE            TO  UT2-KDVALISO                             
337900       MOVE ZERO             TO  UT2-DAINLINL                             
338000       MOVE ZERO             TO  UT2-KVANTMOT                             
338100       MOVE ZERO             TO  UT2-KVSKROT                              
338200       MOVE ZERO             TO  UT2-PRAVCOST                             
338300       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
338400       MOVE ZERO             TO  UT2-KVLS-OLD                             
338500       MOVE SPACE            TO  UT2-FLSLUT                               
338600       MOVE ZERO             TO  UT2-REMARKUP                             
338700       MOVE ZERO             TO  UT2-IDKNOTNR                             
338800       MOVE ZERO             TO  UT2-DAKRENOT                             
338900       MOVE ZERO             TO  UT2-SUKREUTL                             
339000       MOVE ZERO             TO  UT2-SUKRENTO                             
339100       MOVE ZERO             TO  UT2-PRLANDCO                             
339200       MOVE ZERO             TO  UT2-SUKRENOT                             
339300       MOVE ZERO             TO  UT2-KVKREANT                             
339400       MOVE ZERO             TO  UT2-DARETILL                             
339500       MOVE ZERO             TO  UT2-KVLEVANM                             
339600       MOVE ZERO             TO  UT2-KVRETINL                             
339700       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
339800       MOVE ZERO             TO  UT2-IDKONTO                              
339900       MOVE SPACE            TO  UT2-IDKST                                
340000       MOVE ZERO             TO  UT2-DAJUSTDA                             
340100       MOVE ZERO             TO  UT2-KVJUSTKV                             
340200       MOVE ZERO             TO  UT2-KDINVKAT                             
340300       MOVE SPACE            TO  UT2-TEINVANM                             
340400       MOVE 2A10-IDPRODNR    TO  UT2-IDPRODNR                             
340500       MOVE 2A10-IDLEVNR     TO  UT2-IDLEVNR                              
340600       MOVE 2A10-KVBEART     TO  UT2-KVBEART                              
340700       MOVE 2A10-DAORDDAT    TO  UT2-DAORDDAT                             
340800       MOVE 2A10-PRARTBEU    TO  UT2-PRARTBEU                             
340900       MOVE 2A10-PRARTNTO-GNB TO UT2-PRARTNTO-GNB                         
341000       MOVE 2A10-IDFAKT-GNB  TO  UT2-IDFAKT-GNB                           
341100       MOVE 2A10-DAFAKT-GNB  TO  UT2-DAFAKT-GNB                           
341200       MOVE SPACE            TO  UT2-IDFS                                 
341300       MOVE ZERO             TO  UT2-KVAVIS                               
341400       MOVE ZERO             TO  UT2-IDBYTRAP                             
341500       MOVE ZERO             TO  UT2-KVRETUR                              
341600       MOVE ZERO             TO  UT2-SUAVCOST                             
341700       MOVE SPACE            TO  UT2-IDUSER                               
341800       MOVE SPACE            TO  UT2-KDAVCOST                             
341900       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
342000       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
342100       MOVE ZERO             TO  UT2-KVEFRS                               
342200       MOVE ZERO             TO  UT2-KVLS                                 
342300       MOVE ZERO             TO  UT2-DAREGDAT                             
342400       MOVE ZERO             TO  UT2-DASTADAT                             
342500       MOVE ZERO             TO  UT2-PRKURS-SU                            
342600       MOVE ZERO             TO  UT2-PRKURS-SC                            
342700       MOVE ZERO             TO  UT2-PRKURS-UC                            
342800       MOVE ZERO             TO  UT2-PRKURS-CU                            
342810       PERFORM S12-WRITE-W56023A                                          
342900     END-IF                                                               
343000     IF IN2-IDPTYP = 'A11'                                                
343100       MOVE 2A11-IDPTYP      TO  UT2-IDPTYP                               
343200       MOVE 2A11-KDEKOHT     TO  UT2-KDEKOHT                              
343300       MOVE 2A11-IDFTG       TO  UT2-IDFTG                                
343400       MOVE 2A11-IDDC-SEND   TO  UT2-IDDC-SEND                            
343500       MOVE 2A11-IDDC-REC    TO  UT2-IDDC-REC                             
343600       MOVE ZERO             TO  UT2-IDDISTR                              
343700       MOVE ZERO             TO  UT2-IDKUNDNR                             
343800       MOVE ZERO             TO  UT2-IDFAKT                               
343900       MOVE SPACE            TO  UT2-KDFAKTYP                             
344000       MOVE ZERO             TO  UT2-DAFAKT                               
344100       MOVE 2A11-IDORDNR7    TO  UT2-IDORDNR7                             
344200       MOVE 2A11-IDARTNR     TO  UT2-IDARTNR                              
344300       MOVE 2A11-KDPRODSL    TO  UT2-KDPRODSL                             
344400       MOVE 2A11-KDPSLLOC    TO  UT2-KDPSLLOC                             
344500       MOVE ZERO             TO  UT2-KVLEVART                             
344600       MOVE ZERO             TO  UT2-PRARTNTO                             
344700       MOVE SPACE            TO  UT2-FLOVRLEV                             
344800       MOVE ZERO             TO  UT2-KDFRAKT                              
344900       MOVE ZERO             TO  UT2-SUFAKTRE                             
345000       MOVE ZERO             TO  UT2-PREMBHNT                             
345100       MOVE ZERO             TO  UT2-PRFRAKT                              
345200       MOVE ZERO             TO  UT2-PRFOERS                              
345300       MOVE ZERO             TO  UT2-PRMOMS                               
345400       MOVE ZERO             TO  UT2-PRLEGKST                             
345500       MOVE ZERO             TO  UT2-SUFKTTILL                            
345600       MOVE ZERO             TO  UT2-PRAVDRAG                             
345700       MOVE ZERO             TO  UT2-SUFKTBEL                             
345800       MOVE ZERO             TO  UT2-SUFKTUTL                             
345900       MOVE ZERO             TO  UT2-PRKURS                               
346000       MOVE ZERO             TO  UT2-IDRAPPNR                             
346100       MOVE ZERO             TO  UT2-IDKOLLI                              
346200       MOVE SPACE            TO  UT2-KDANMORS                             
346300       MOVE SPACE            TO  UT2-KDVALISO                             
346400       MOVE 2A11-DAINLINL    TO  UT2-DAINLINL                             
346500       MOVE 2A11-KVANTMOT    TO  UT2-KVANTMOT                             
346600       MOVE ZERO             TO  UT2-KVSKROT                              
346700       MOVE 2A11-PRAVCOST    TO  UT2-PRAVCOST                             
346800       MOVE 2A11-PRAVCOST-OLD TO UT2-PRAVCOST-OLD                         
346900       MOVE 2A11-KVLS-OLD    TO  UT2-KVLS-OLD                             
347000       MOVE SPACE            TO  UT2-FLSLUT                               
347100       MOVE 2A11-REMARKUP    TO  UT2-REMARKUP                             
347200       MOVE ZERO             TO  UT2-IDKNOTNR                             
347300       MOVE ZERO             TO  UT2-DAKRENOT                             
347400       MOVE ZERO             TO  UT2-SUKREUTL                             
347500       MOVE ZERO             TO  UT2-SUKRENTO                             
347600       MOVE ZERO             TO  UT2-PRLANDCO                             
347700       MOVE ZERO             TO  UT2-SUKRENOT                             
347800       MOVE ZERO             TO  UT2-KVKREANT                             
347900       MOVE ZERO             TO  UT2-DARETILL                             
348000       MOVE ZERO             TO  UT2-KVLEVANM                             
348100       MOVE ZERO             TO  UT2-KVRETINL                             
348200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
348300       MOVE ZERO             TO  UT2-IDKONTO                              
348400       MOVE SPACE            TO  UT2-IDKST                                
348500       MOVE ZERO             TO  UT2-DAJUSTDA                             
348600       MOVE ZERO             TO  UT2-KVJUSTKV                             
348700       MOVE ZERO             TO  UT2-KDINVKAT                             
348800       MOVE SPACE            TO  UT2-TEINVANM                             
348900       MOVE ZERO             TO  UT2-IDPRODNR                             
349000       MOVE 2A11-IDLEVNR     TO  UT2-IDLEVNR                              
349100       MOVE ZERO             TO  UT2-KVBEART                              
349200       MOVE ZERO             TO  UT2-DAORDDAT                             
349300       MOVE 2A11-PRARTBEU    TO  UT2-PRARTBEU                             
349400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
349500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
349600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
349700       MOVE 2A11-IDFS        TO  UT2-IDFS                                 
349800       MOVE 2A11-KVAVIS      TO  UT2-KVAVIS                               
349900       MOVE ZERO             TO  UT2-IDBYTRAP                             
350000       MOVE ZERO             TO  UT2-KVRETUR                              
350100       MOVE ZERO             TO  UT2-SUAVCOST                             
350200       MOVE SPACE            TO  UT2-IDUSER                               
350300       MOVE SPACE            TO  UT2-KDAVCOST                             
350400       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
350500       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
350600       MOVE ZERO             TO  UT2-KVEFRS                               
350700       MOVE ZERO             TO  UT2-KVLS                                 
350800       MOVE ZERO             TO  UT2-DAREGDAT                             
350900       MOVE ZERO             TO  UT2-DASTADAT                             
351000       MOVE ZERO             TO  UT2-PRKURS-SU                            
351100       MOVE ZERO             TO  UT2-PRKURS-SC                            
351200       MOVE ZERO             TO  UT2-PRKURS-UC                            
351300       MOVE ZERO             TO  UT2-PRKURS-CU                            
351310       PERFORM S12-WRITE-W56023A                                          
351400     END-IF                                                               
351500     IF IN2-IDPTYP = 'A12'                                                
351600       MOVE 2A12-IDPTYP      TO  UT2-IDPTYP                               
351700       MOVE 2A12-KDEKOHT     TO  UT2-KDEKOHT                              
351800       MOVE 2A12-IDFTG       TO  UT2-IDFTG                                
351900       MOVE 2A12-IDDC-SEND   TO  UT2-IDDC-SEND                            
352000       MOVE 2A12-IDDC-REC    TO  UT2-IDDC-REC                             
352100       MOVE 2A12-IDDISTR     TO  UT2-IDDISTR                              
352200       MOVE 2A12-IDKUNDNR    TO  UT2-IDKUNDNR                             
352300       MOVE ZERO             TO  UT2-IDFAKT                               
352400       MOVE SPACE            TO  UT2-KDFAKTYP                             
352500       MOVE ZERO             TO  UT2-DAFAKT                               
352600       MOVE ZERO             TO  UT2-IDORDNR7                             
352700       MOVE 2A12-IDARTNR     TO  UT2-IDARTNR                              
352800       MOVE 2A12-KDPRODSL    TO  UT2-KDPRODSL                             
352900       MOVE 2A12-KDPSLLOC    TO  UT2-KDPSLLOC                             
353000       MOVE ZERO             TO  UT2-KVLEVART                             
353100       MOVE ZERO             TO  UT2-PRARTNTO                             
353200       MOVE SPACE            TO  UT2-FLOVRLEV                             
353300       MOVE ZERO             TO  UT2-KDFRAKT                              
353400       MOVE ZERO             TO  UT2-SUFAKTRE                             
353500       MOVE ZERO             TO  UT2-PREMBHNT                             
353600       MOVE ZERO             TO  UT2-PRFRAKT                              
353700       MOVE ZERO             TO  UT2-PRFOERS                              
353800       MOVE ZERO             TO  UT2-PRMOMS                               
353900       MOVE ZERO             TO  UT2-PRLEGKST                             
354000       MOVE ZERO             TO  UT2-SUFKTTILL                            
354100       MOVE ZERO             TO  UT2-PRAVDRAG                             
354200       MOVE ZERO             TO  UT2-SUFKTBEL                             
354300       MOVE ZERO             TO  UT2-SUFKTUTL                             
354400       MOVE ZERO             TO  UT2-PRKURS                               
354500       MOVE 2A12-IDRAPPNR    TO  UT2-IDRAPPNR                             
354600       MOVE ZERO             TO  UT2-IDKOLLI                              
354700       MOVE 2A12-KDANMORS    TO  UT2-KDANMORS                             
354800       MOVE SPACE            TO  UT2-KDVALISO                             
354900       MOVE ZERO             TO  UT2-DAINLINL                             
355000       MOVE ZERO             TO  UT2-KVANTMOT                             
355100       MOVE ZERO             TO  UT2-KVSKROT                              
355200       MOVE 2A12-PRAVCOST    TO  UT2-PRAVCOST                             
355300       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
355400       MOVE ZERO             TO  UT2-KVLS-OLD                             
355500       MOVE SPACE            TO  UT2-FLSLUT                               
355600       MOVE ZERO             TO  UT2-REMARKUP                             
355700       MOVE ZERO             TO  UT2-IDKNOTNR                             
355800       MOVE ZERO             TO  UT2-DAKRENOT                             
355900       MOVE ZERO             TO  UT2-SUKREUTL                             
356000       MOVE ZERO             TO  UT2-SUKRENTO                             
356100       MOVE ZERO             TO  UT2-PRLANDCO                             
356200       MOVE ZERO             TO  UT2-SUKRENOT                             
356300       MOVE ZERO             TO  UT2-KVKREANT                             
356400       MOVE ZERO             TO  UT2-DARETILL                             
356500       MOVE ZERO             TO  UT2-KVLEVANM                             
356600       MOVE ZERO             TO  UT2-KVRETINL                             
356700       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
356800       MOVE ZERO             TO  UT2-IDKONTO                              
356900       MOVE SPACE            TO  UT2-IDKST                                
357000       MOVE 2A12-DAJUSTDA    TO  UT2-DAJUSTDA                             
357100       MOVE 2A12-KVJUSTKV    TO  UT2-KVJUSTKV                             
357200       MOVE ZERO             TO  UT2-KDINVKAT                             
357300       MOVE SPACE            TO  UT2-TEINVANM                             
357400       MOVE ZERO             TO  UT2-IDPRODNR                             
357500       MOVE SPACE            TO  UT2-IDLEVNR                              
357600       MOVE ZERO             TO  UT2-KVBEART                              
357700       MOVE ZERO             TO  UT2-DAORDDAT                             
357800       MOVE ZERO             TO  UT2-PRARTBEU                             
357900       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
358000       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
358100       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
358200       MOVE SPACE            TO  UT2-IDFS                                 
358300       MOVE ZERO             TO  UT2-KVAVIS                               
358400       MOVE ZERO             TO  UT2-IDBYTRAP                             
358500       MOVE ZERO             TO  UT2-KVRETUR                              
358600       MOVE ZERO             TO  UT2-SUAVCOST                             
358700       MOVE SPACE            TO  UT2-IDUSER                               
358800       MOVE SPACE            TO  UT2-KDAVCOST                             
358900       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
359000       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
359100       MOVE ZERO             TO  UT2-KVEFRS                               
359200       MOVE ZERO             TO  UT2-KVLS                                 
359300       MOVE ZERO             TO  UT2-DAREGDAT                             
359400       MOVE ZERO             TO  UT2-DASTADAT                             
359500       MOVE ZERO             TO  UT2-PRKURS-SU                            
359600       MOVE ZERO             TO  UT2-PRKURS-SC                            
359700       MOVE ZERO             TO  UT2-PRKURS-UC                            
359800       MOVE ZERO             TO  UT2-PRKURS-CU                            
359810       PERFORM S12-WRITE-W56023A                                          
359900     END-IF                                                               
360000     IF IN2-IDPTYP = 'A13'                                                
360100       MOVE 2A13-IDPTYP      TO  UT2-IDPTYP                               
360200       MOVE 2A13-KDEKOHT     TO  UT2-KDEKOHT                              
360300       MOVE 2A13-IDFTG       TO  UT2-IDFTG                                
360400       MOVE 2A13-IDDC-SEND   TO  UT2-IDDC-SEND                            
360500       MOVE 2A13-IDDC-REC    TO  UT2-IDDC-REC                             
360600       MOVE 2A13-IDDISTR     TO  UT2-IDDISTR                              
360700       MOVE 2A13-IDKUNDNR    TO  UT2-IDKUNDNR                             
360800       MOVE 2A13-IDFAKT      TO  UT2-IDFAKT                               
360900       MOVE SPACE            TO  UT2-KDFAKTYP                             
361000       MOVE 2A13-DAFAKT      TO  UT2-DAFAKT                               
361100       MOVE 2A13-IDORDNR7    TO  UT2-IDORDNR7                             
361200       MOVE 2A13-IDARTNR     TO  UT2-IDARTNR                              
361300       MOVE 2A13-KDPRODSL    TO  UT2-KDPRODSL                             
361400       MOVE 2A13-KDPSLLOC    TO  UT2-KDPSLLOC                             
361500       MOVE 2A13-KVLEVART    TO  UT2-KVLEVART                             
361600       MOVE 2A13-PRARTNTO    TO  UT2-PRARTNTO                             
361700       MOVE SPACE            TO  UT2-FLOVRLEV                             
361800       MOVE ZERO             TO  UT2-KDFRAKT                              
361900       MOVE ZERO             TO  UT2-SUFAKTRE                             
362000       MOVE ZERO             TO  UT2-PREMBHNT                             
362100       MOVE ZERO             TO  UT2-PRFRAKT                              
362200       MOVE ZERO             TO  UT2-PRFOERS                              
362300       MOVE ZERO             TO  UT2-PRMOMS                               
362400       MOVE ZERO             TO  UT2-PRLEGKST                             
362500       MOVE ZERO             TO  UT2-SUFKTTILL                            
362600       MOVE ZERO             TO  UT2-PRAVDRAG                             
362700       MOVE ZERO             TO  UT2-SUFKTBEL                             
362800       MOVE ZERO             TO  UT2-SUFKTUTL                             
362900       MOVE 2A13-PRKURS      TO  UT2-PRKURS                               
363000       MOVE ZERO             TO  UT2-IDRAPPNR                             
363100       MOVE ZERO             TO  UT2-IDKOLLI                              
363200       MOVE SPACE            TO  UT2-KDANMORS                             
363300       MOVE SPACE            TO  UT2-KDVALISO                             
363400       MOVE ZERO             TO  UT2-DAINLINL                             
363500       MOVE ZERO             TO  UT2-KVANTMOT                             
363600       MOVE ZERO             TO  UT2-KVSKROT                              
363700       MOVE 2A13-PRAVCOST    TO  UT2-PRAVCOST                             
363800       MOVE 2A13-PRAVCOST-OLD TO UT2-PRAVCOST-OLD                         
363900       MOVE 2A13-KVLS-OLD    TO  UT2-KVLS-OLD                             
364000       MOVE SPACE            TO  UT2-FLSLUT                               
364100       MOVE 2A13-REMARKUP    TO  UT2-REMARKUP                             
364200       MOVE ZERO             TO  UT2-IDKNOTNR                             
364300       MOVE ZERO             TO  UT2-DAKRENOT                             
364400       MOVE ZERO             TO  UT2-SUKREUTL                             
364500       MOVE ZERO             TO  UT2-SUKRENTO                             
364600       MOVE ZERO             TO  UT2-PRLANDCO                             
364700       MOVE ZERO             TO  UT2-SUKRENOT                             
364800       MOVE ZERO             TO  UT2-KVKREANT                             
364900       MOVE ZERO             TO  UT2-DARETILL                             
365000       MOVE ZERO             TO  UT2-KVLEVANM                             
365100       MOVE ZERO             TO  UT2-KVRETINL                             
365200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
365300       MOVE ZERO             TO  UT2-IDKONTO                              
365400       MOVE SPACE            TO  UT2-IDKST                                
365500       MOVE ZERO             TO  UT2-DAJUSTDA                             
365600       MOVE ZERO             TO  UT2-KVJUSTKV                             
365700       MOVE ZERO             TO  UT2-KDINVKAT                             
365800       MOVE SPACE            TO  UT2-TEINVANM                             
365900       MOVE ZERO             TO  UT2-IDPRODNR                             
366000       MOVE SPACE            TO  UT2-IDLEVNR                              
366100       MOVE ZERO             TO  UT2-KVBEART                              
366200       MOVE ZERO             TO  UT2-DAORDDAT                             
366300       MOVE ZERO             TO  UT2-PRARTBEU                             
366400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
366500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
366600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
366700       MOVE SPACE            TO  UT2-IDFS                                 
366800       MOVE ZERO             TO  UT2-KVAVIS                               
366900       MOVE ZERO             TO  UT2-IDBYTRAP                             
367000       MOVE ZERO             TO  UT2-KVRETUR                              
367100       MOVE ZERO             TO  UT2-SUAVCOST                             
367200       MOVE SPACE            TO  UT2-IDUSER                               
367300       MOVE SPACE            TO  UT2-KDAVCOST                             
367400       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
367500       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
367600       MOVE ZERO             TO  UT2-KVEFRS                               
367700       MOVE ZERO             TO  UT2-KVLS                                 
367800       MOVE ZERO             TO  UT2-DAREGDAT                             
367900       MOVE ZERO             TO  UT2-DASTADAT                             
368000       MOVE ZERO             TO  UT2-PRKURS-SU                            
368100       MOVE ZERO             TO  UT2-PRKURS-SC                            
368200       MOVE ZERO             TO  UT2-PRKURS-UC                            
368300       MOVE ZERO             TO  UT2-PRKURS-CU                            
368310       PERFORM S12-WRITE-W56023A                                          
368400     END-IF                                                               
368500     IF IN2-IDPTYP = 'A14'                                                
368600       MOVE 2A14-IDPTYP      TO  UT2-IDPTYP                               
368700       MOVE 2A14-KDEKOHT     TO  UT2-KDEKOHT                              
368800       MOVE 2A14-IDFTG       TO  UT2-IDFTG                                
368900       MOVE 2A14-IDDC-SEND   TO  UT2-IDDC-SEND                            
369000       MOVE 2A14-IDDC-REC    TO  UT2-IDDC-REC                             
369100       MOVE 2A14-IDDISTR     TO  UT2-IDDISTR                              
369200       MOVE 2A14-IDKUNDNR    TO  UT2-IDKUNDNR                             
369300       MOVE ZERO             TO  UT2-IDFAKT                               
369400       MOVE SPACE            TO  UT2-KDFAKTYP                             
369500       MOVE ZERO             TO  UT2-DAFAKT                               
369600       MOVE ZERO             TO  UT2-IDORDNR7                             
369700       MOVE 2A14-IDARTNR     TO  UT2-IDARTNR                              
369800       MOVE 2A14-KDPRODSL    TO  UT2-KDPRODSL                             
369900       MOVE 2A14-KDPSLLOC    TO  UT2-KDPSLLOC                             
370000       MOVE ZERO             TO  UT2-KVLEVART                             
370100       MOVE ZERO             TO  UT2-PRARTNTO                             
370200       MOVE SPACE            TO  UT2-FLOVRLEV                             
370300       MOVE ZERO             TO  UT2-KDFRAKT                              
370400       MOVE ZERO             TO  UT2-SUFAKTRE                             
370500       MOVE ZERO             TO  UT2-PREMBHNT                             
370600       MOVE ZERO             TO  UT2-PRFRAKT                              
370700       MOVE ZERO             TO  UT2-PRFOERS                              
370800       MOVE ZERO             TO  UT2-PRMOMS                               
370900       MOVE ZERO             TO  UT2-PRLEGKST                             
371000       MOVE ZERO             TO  UT2-SUFKTTILL                            
371100       MOVE ZERO             TO  UT2-PRAVDRAG                             
371200       MOVE ZERO             TO  UT2-SUFKTBEL                             
371300       MOVE ZERO             TO  UT2-SUFKTUTL                             
371400       MOVE ZERO             TO  UT2-PRKURS                               
371500       MOVE ZERO             TO  UT2-IDRAPPNR                             
371600       MOVE ZERO             TO  UT2-IDKOLLI                              
371700       MOVE SPACE            TO  UT2-KDANMORS                             
371800       MOVE SPACE            TO  UT2-KDVALISO                             
371900       MOVE 2A14-DAINLINL    TO  UT2-DAINLINL                             
372000       MOVE ZERO             TO  UT2-KVANTMOT                             
372100       MOVE ZERO             TO  UT2-KVSKROT                              
372200       MOVE 2A14-PRAVCOST    TO  UT2-PRAVCOST                             
372300       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
372400       MOVE ZERO             TO  UT2-KVLS-OLD                             
372500       MOVE SPACE            TO  UT2-FLSLUT                               
372600       MOVE ZERO             TO  UT2-REMARKUP                             
372700       MOVE ZERO             TO  UT2-IDKNOTNR                             
372800       MOVE ZERO             TO  UT2-DAKRENOT                             
372900       MOVE ZERO             TO  UT2-SUKREUTL                             
373000       MOVE ZERO             TO  UT2-SUKRENTO                             
373100       MOVE ZERO             TO  UT2-PRLANDCO                             
373200       MOVE ZERO             TO  UT2-SUKRENOT                             
373300       MOVE ZERO             TO  UT2-KVKREANT                             
373400       MOVE ZERO             TO  UT2-DARETILL                             
373500       MOVE ZERO             TO  UT2-KVLEVANM                             
373600       MOVE ZERO             TO  UT2-KVRETINL                             
373700       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
373800       MOVE ZERO             TO  UT2-IDKONTO                              
373900       MOVE SPACE            TO  UT2-IDKST                                
374000       MOVE ZERO             TO  UT2-DAJUSTDA                             
374100       MOVE ZERO             TO  UT2-KVJUSTKV                             
374200       MOVE ZERO             TO  UT2-KDINVKAT                             
374300       MOVE SPACE            TO  UT2-TEINVANM                             
374400       MOVE ZERO             TO  UT2-IDPRODNR                             
374500       MOVE SPACE            TO  UT2-IDLEVNR                              
374600       MOVE ZERO             TO  UT2-KVBEART                              
374700       MOVE ZERO             TO  UT2-DAORDDAT                             
374800       MOVE ZERO             TO  UT2-PRARTBEU                             
374900       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
375000       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
375100       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
375200       MOVE SPACE            TO  UT2-IDFS                                 
375300       MOVE ZERO             TO  UT2-KVAVIS                               
375400       MOVE 2A14-IDBYTRAP    TO  UT2-IDBYTRAP                             
375500       MOVE 2A14-KVRETUR     TO  UT2-KVRETUR                              
375600       MOVE ZERO             TO  UT2-SUAVCOST                             
375700       MOVE SPACE            TO  UT2-IDUSER                               
375800       MOVE SPACE            TO  UT2-KDAVCOST                             
375900       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
376000       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
376100       MOVE ZERO             TO  UT2-KVEFRS                               
376200       MOVE ZERO             TO  UT2-KVLS                                 
376300       MOVE ZERO             TO  UT2-DAREGDAT                             
376400       MOVE ZERO             TO  UT2-DASTADAT                             
376500       MOVE ZERO             TO  UT2-PRKURS-SU                            
376600       MOVE ZERO             TO  UT2-PRKURS-SC                            
376700       MOVE ZERO             TO  UT2-PRKURS-UC                            
376800       MOVE ZERO             TO  UT2-PRKURS-CU                            
376810       PERFORM S12-WRITE-W56023A                                          
376900     END-IF                                                               
377000     IF IN2-IDPTYP = 'A15'                                                
377100       MOVE 2A15-IDPTYP      TO  UT2-IDPTYP                               
377200       MOVE 2A15-KDEKOHT     TO  UT2-KDEKOHT                              
377300       MOVE 2A15-IDFTG       TO  UT2-IDFTG                                
377400       MOVE 2A15-IDDC-SEND   TO  UT2-IDDC-SEND                            
377500       MOVE 2A15-IDDC-REC    TO  UT2-IDDC-REC                             
377600       MOVE 2A15-IDDISTR     TO  UT2-IDDISTR                              
377700       MOVE 2A15-IDKUNDNR    TO  UT2-IDKUNDNR                             
377800       MOVE 2A15-IDFAKT      TO  UT2-IDFAKT                               
377900       MOVE SPACE            TO  UT2-KDFAKTYP                             
378000       MOVE 2A15-DAFAKT      TO  UT2-DAFAKT                               
378100       MOVE ZERO             TO  UT2-IDORDNR7                             
378200       MOVE 2A15-IDARTNR     TO  UT2-IDARTNR                              
378300       MOVE 2A15-KDPRODSL    TO  UT2-KDPRODSL                             
378400       MOVE 2A15-KDPSLLOC    TO  UT2-KDPSLLOC                             
378500       MOVE 2A15-KVLEVART    TO  UT2-KVLEVART                             
378600       MOVE ZERO             TO  UT2-PRARTNTO                             
378700       MOVE SPACE            TO  UT2-FLOVRLEV                             
378800       MOVE ZERO             TO  UT2-KDFRAKT                              
378900       MOVE ZERO             TO  UT2-SUFAKTRE                             
379000       MOVE ZERO             TO  UT2-PREMBHNT                             
379100       MOVE ZERO             TO  UT2-PRFRAKT                              
379200       MOVE ZERO             TO  UT2-PRFOERS                              
379300       MOVE ZERO             TO  UT2-PRMOMS                               
379400       MOVE ZERO             TO  UT2-PRLEGKST                             
379500       MOVE ZERO             TO  UT2-SUFKTTILL                            
379600       MOVE ZERO             TO  UT2-PRAVDRAG                             
379700       MOVE ZERO             TO  UT2-SUFKTBEL                             
379800       MOVE ZERO             TO  UT2-SUFKTUTL                             
379900       MOVE ZERO             TO  UT2-PRKURS                               
380000       MOVE ZERO             TO  UT2-IDRAPPNR                             
380100       MOVE ZERO             TO  UT2-IDKOLLI                              
380200       MOVE SPACE            TO  UT2-KDANMORS                             
380300       MOVE SPACE            TO  UT2-KDVALISO                             
380400       MOVE ZERO             TO  UT2-DAINLINL                             
380500       MOVE ZERO             TO  UT2-KVANTMOT                             
380600       MOVE ZERO             TO  UT2-KVSKROT                              
380700       MOVE 2A15-PRAVCOST    TO  UT2-PRAVCOST                             
380800       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
380900       MOVE ZERO             TO  UT2-KVLS-OLD                             
381000       MOVE SPACE            TO  UT2-FLSLUT                               
381100       MOVE ZERO             TO  UT2-REMARKUP                             
381200       MOVE ZERO             TO  UT2-IDKNOTNR                             
381300       MOVE ZERO             TO  UT2-DAKRENOT                             
381400       MOVE ZERO             TO  UT2-SUKREUTL                             
381500       MOVE ZERO             TO  UT2-SUKRENTO                             
381600       MOVE ZERO             TO  UT2-PRLANDCO                             
381700       MOVE ZERO             TO  UT2-SUKRENOT                             
381800       MOVE ZERO             TO  UT2-KVKREANT                             
381900       MOVE ZERO             TO  UT2-DARETILL                             
382000       MOVE ZERO             TO  UT2-KVLEVANM                             
382100       MOVE ZERO             TO  UT2-KVRETINL                             
382200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
382300       MOVE ZERO             TO  UT2-IDKONTO                              
382400       MOVE SPACE            TO  UT2-IDKST                                
382500       MOVE ZERO             TO  UT2-DAJUSTDA                             
382600       MOVE ZERO             TO  UT2-KVJUSTKV                             
382700       MOVE ZERO             TO  UT2-KDINVKAT                             
382800       MOVE SPACE            TO  UT2-TEINVANM                             
382900       MOVE ZERO             TO  UT2-IDPRODNR                             
383000       MOVE SPACE            TO  UT2-IDLEVNR                              
383100       MOVE ZERO             TO  UT2-KVBEART                              
383200       MOVE ZERO             TO  UT2-DAORDDAT                             
383300       MOVE ZERO             TO  UT2-PRARTBEU                             
383400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
383500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
383600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
383700       MOVE SPACE            TO  UT2-IDFS                                 
383800       MOVE ZERO             TO  UT2-KVAVIS                               
383900       MOVE ZERO             TO  UT2-IDBYTRAP                             
384000       MOVE ZERO             TO  UT2-KVRETUR                              
384100       MOVE ZERO             TO  UT2-SUAVCOST                             
384200       MOVE SPACE            TO  UT2-IDUSER                               
384300       MOVE SPACE            TO  UT2-KDAVCOST                             
384400       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
384500       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
384600       MOVE ZERO             TO  UT2-KVEFRS                               
384700       MOVE ZERO             TO  UT2-KVLS                                 
384800       MOVE ZERO             TO  UT2-DAREGDAT                             
384900       MOVE ZERO             TO  UT2-DASTADAT                             
385000       MOVE ZERO             TO  UT2-PRKURS-SU                            
385100       MOVE ZERO             TO  UT2-PRKURS-SC                            
385200       MOVE ZERO             TO  UT2-PRKURS-UC                            
385300       MOVE ZERO             TO  UT2-PRKURS-CU                            
385310       PERFORM S12-WRITE-W56023A                                          
385400     END-IF                                                               
385500     IF IN2-IDPTYP = 'A16'                                                
385600       MOVE 2A16-IDPTYP     TO  UT2-IDPTYP                                
385700       MOVE 2A16-KDEKOHT     TO  UT2-KDEKOHT                              
385800       MOVE 2A16-IDFTG       TO  UT2-IDFTG                                
385900       MOVE 2A16-IDDC-SEND   TO  UT2-IDDC-SEND                            
386000       MOVE 2A16-IDDC-REC    TO  UT2-IDDC-REC                             
386100       MOVE ZERO             TO  UT2-IDDISTR                              
386200       MOVE ZERO             TO  UT2-IDKUNDNR                             
386300       MOVE ZERO             TO  UT2-IDFAKT                               
386400       MOVE SPACE            TO  UT2-KDFAKTYP                             
386500       MOVE ZERO             TO  UT2-DAFAKT                               
386600       MOVE ZERO             TO  UT2-IDORDNR7                             
386700       MOVE 2A16-IDARTNR     TO  UT2-IDARTNR                              
386800       MOVE 2A16-KDPRODSL    TO  UT2-KDPRODSL                             
386900       MOVE 2A16-KDPSLLOC    TO  UT2-KDPSLLOC                             
387000       MOVE ZERO             TO  UT2-KVLEVART                             
387100       MOVE ZERO             TO  UT2-PRARTNTO                             
387200       MOVE SPACE            TO  UT2-FLOVRLEV                             
387300       MOVE ZERO             TO  UT2-KDFRAKT                              
387400       MOVE ZERO             TO  UT2-SUFAKTRE                             
387500       MOVE ZERO             TO  UT2-PREMBHNT                             
387600       MOVE ZERO             TO  UT2-PRFRAKT                              
387700       MOVE ZERO             TO  UT2-PRFOERS                              
387800       MOVE ZERO             TO  UT2-PRMOMS                               
387900       MOVE ZERO             TO  UT2-PRLEGKST                             
388000       MOVE ZERO             TO  UT2-SUFKTTILL                            
388100       MOVE ZERO             TO  UT2-PRAVDRAG                             
388200       MOVE ZERO             TO  UT2-SUFKTBEL                             
388300       MOVE ZERO             TO  UT2-SUFKTUTL                             
388400       MOVE ZERO             TO  UT2-PRKURS                               
388500       MOVE ZERO             TO  UT2-IDRAPPNR                             
388600       MOVE ZERO             TO  UT2-IDKOLLI                              
388700       MOVE SPACE            TO  UT2-KDANMORS                             
388800       MOVE SPACE            TO  UT2-KDVALISO                             
388900       MOVE ZERO             TO  UT2-DAINLINL                             
389000       MOVE ZERO             TO  UT2-KVANTMOT                             
389100       MOVE ZERO             TO  UT2-KVSKROT                              
389200       MOVE 2A16-PRAVCOST    TO  UT2-PRAVCOST                             
389300       MOVE 2A16-PRAVCOST-OLD TO UT2-PRAVCOST-OLD                         
389400       MOVE ZERO             TO  UT2-KVLS-OLD                             
389500       MOVE SPACE            TO  UT2-FLSLUT                               
389600       MOVE ZERO             TO  UT2-REMARKUP                             
389700       MOVE ZERO             TO  UT2-IDKNOTNR                             
389800       MOVE ZERO             TO  UT2-DAKRENOT                             
389900       MOVE ZERO             TO  UT2-SUKREUTL                             
390000       MOVE ZERO             TO  UT2-SUKRENTO                             
390100       MOVE ZERO             TO  UT2-PRLANDCO                             
390200       MOVE ZERO             TO  UT2-SUKRENOT                             
390300       MOVE ZERO             TO  UT2-KVKREANT                             
390400       MOVE ZERO             TO  UT2-DARETILL                             
390500       MOVE ZERO             TO  UT2-KVLEVANM                             
390600       MOVE ZERO             TO  UT2-KVRETINL                             
390700       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
390800       MOVE ZERO             TO  UT2-IDKONTO                              
390900       MOVE SPACE            TO  UT2-IDKST                                
391000       MOVE 2A16-DAJUSTDA    TO  UT2-DAJUSTDA                             
391100       MOVE ZERO             TO  UT2-KVJUSTKV                             
391200       MOVE ZERO             TO  UT2-KDINVKAT                             
391300       MOVE SPACE            TO  UT2-TEINVANM                             
391400       MOVE ZERO             TO  UT2-IDPRODNR                             
391500       MOVE SPACE            TO  UT2-IDLEVNR                              
391600       MOVE ZERO             TO  UT2-KVBEART                              
391700       MOVE ZERO             TO  UT2-DAORDDAT                             
391800       MOVE ZERO             TO  UT2-PRARTBEU                             
391900       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
392000       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
392100       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
392200       MOVE 2A16-IDFS        TO  UT2-IDFS                                 
392300       MOVE ZERO             TO  UT2-KVAVIS                               
392400       MOVE ZERO             TO  UT2-IDBYTRAP                             
392500       MOVE ZERO             TO  UT2-KVRETUR                              
392600       MOVE 2A16-SUAVCOST    TO  UT2-SUAVCOST                             
392700       MOVE 2A16-IDUSER      TO  UT2-IDUSER                               
392800       MOVE 2A16-KDAVCOST    TO  UT2-KDAVCOST                             
392900       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
393000       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
393100       MOVE ZERO             TO  UT2-KVEFRS                               
393200       MOVE 2A16-KVLS        TO  UT2-KVLS                                 
393300       MOVE ZERO             TO  UT2-DAREGDAT                             
393400       MOVE ZERO             TO  UT2-DASTADAT                             
393500       MOVE ZERO             TO  UT2-PRKURS-SU                            
393600       MOVE ZERO             TO  UT2-PRKURS-SC                            
393700       MOVE ZERO             TO  UT2-PRKURS-UC                            
393800       MOVE ZERO             TO  UT2-PRKURS-CU                            
393810       PERFORM S12-WRITE-W56023A                                          
393900     END-IF                                                               
394000     IF IN2-IDPTYP = 'A17'                                                
394100       MOVE 2A17-IDPTYP      TO  UT2-IDPTYP                               
394200       MOVE 2A17-KDEKOHT     TO  UT2-KDEKOHT                              
394300       MOVE 2A17-IDFTG       TO  UT2-IDFTG                                
394400       MOVE 2A17-IDDC-SEND   TO  UT2-IDDC-SEND                            
394500       MOVE 2A17-IDDC-REC    TO  UT2-IDDC-REC                             
394600       MOVE ZERO             TO  UT2-IDDISTR                              
394700       MOVE ZERO             TO  UT2-IDKUNDNR                             
394800       MOVE ZERO             TO  UT2-IDFAKT                               
394900       MOVE SPACE            TO  UT2-KDFAKTYP                             
395000       MOVE ZERO             TO  UT2-DAFAKT                               
395100       MOVE ZERO             TO  UT2-IDORDNR7                             
395200       MOVE 2A17-IDARTNR     TO  UT2-IDARTNR                              
395300       MOVE 2A17-KDPRODSL    TO  UT2-KDPRODSL                             
395400       MOVE ZERO             TO  UT2-KDPSLLOC                             
395500       MOVE ZERO             TO  UT2-KVLEVART                             
395600       MOVE ZERO             TO  UT2-PRARTNTO                             
395700       MOVE SPACE            TO  UT2-FLOVRLEV                             
395800       MOVE ZERO             TO  UT2-KDFRAKT                              
395900       MOVE ZERO             TO  UT2-SUFAKTRE                             
396000       MOVE ZERO             TO  UT2-PREMBHNT                             
396100       MOVE ZERO             TO  UT2-PRFRAKT                              
396200       MOVE ZERO             TO  UT2-PRFOERS                              
396300       MOVE ZERO             TO  UT2-PRMOMS                               
396400       MOVE ZERO             TO  UT2-PRLEGKST                             
396500       MOVE ZERO             TO  UT2-SUFKTTILL                            
396600       MOVE ZERO             TO  UT2-PRAVDRAG                             
396700       MOVE ZERO             TO  UT2-SUFKTBEL                             
396800       MOVE ZERO             TO  UT2-SUFKTUTL                             
396900       MOVE ZERO             TO  UT2-PRKURS                               
397000       MOVE ZERO             TO  UT2-IDRAPPNR                             
397100       MOVE ZERO             TO  UT2-IDKOLLI                              
397200       MOVE SPACE            TO  UT2-KDANMORS                             
397300       MOVE SPACE            TO  UT2-KDVALISO                             
397400       MOVE ZERO             TO  UT2-DAINLINL                             
397500       MOVE ZERO             TO  UT2-KVANTMOT                             
397600       MOVE ZERO             TO  UT2-KVSKROT                              
397700       MOVE 2A17-PRAVCOST    TO  UT2-PRAVCOST                             
397800       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
397900       MOVE ZERO             TO  UT2-KVLS-OLD                             
398000       MOVE SPACE            TO  UT2-FLSLUT                               
398100       MOVE ZERO             TO  UT2-REMARKUP                             
398200       MOVE ZERO             TO  UT2-IDKNOTNR                             
398300       MOVE ZERO             TO  UT2-DAKRENOT                             
398400       MOVE ZERO             TO  UT2-SUKREUTL                             
398500       MOVE ZERO             TO  UT2-SUKRENTO                             
398600       MOVE ZERO             TO  UT2-PRLANDCO                             
398700       MOVE ZERO             TO  UT2-SUKRENOT                             
398800       MOVE ZERO             TO  UT2-KVKREANT                             
398900       MOVE ZERO             TO  UT2-DARETILL                             
399000       MOVE ZERO             TO  UT2-KVLEVANM                             
399100       MOVE ZERO             TO  UT2-KVRETINL                             
399200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
399300       MOVE ZERO             TO  UT2-IDKONTO                              
399400       MOVE SPACE            TO  UT2-IDKST                                
399500       MOVE 2A17-DAJUSTDA    TO  UT2-DAJUSTDA                             
399600       MOVE ZERO             TO  UT2-KVJUSTKV                             
399700       MOVE ZERO             TO  UT2-KDINVKAT                             
399800       MOVE SPACE            TO  UT2-TEINVANM                             
399900       MOVE ZERO             TO  UT2-IDPRODNR                             
400000       MOVE SPACE            TO  UT2-IDLEVNR                              
400100       MOVE ZERO             TO  UT2-KVBEART                              
400200       MOVE ZERO             TO  UT2-DAORDDAT                             
400300       MOVE ZERO             TO  UT2-PRARTBEU                             
400400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
400500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
400600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
400700       MOVE SPACE            TO  UT2-IDFS                                 
400800       MOVE ZERO             TO  UT2-KVAVIS                               
400900       MOVE ZERO             TO  UT2-IDBYTRAP                             
401000       MOVE ZERO             TO  UT2-KVRETUR                              
401100       MOVE ZERO             TO  UT2-SUAVCOST                             
401200       MOVE SPACE            TO  UT2-IDUSER                               
401300       MOVE SPACE            TO  UT2-KDAVCOST                             
401400       MOVE 2A17-KDPSLLOC-NEW TO UT2-KDPSLLOC-NEW                         
401500       MOVE 2A17-KDPSLLOC-OLD TO UT2-KDPSLLOC-OLD                         
401600       MOVE 2A17-KVEFRS      TO  UT2-KVEFRS                               
401700       MOVE 2A17-KVLS        TO  UT2-KVLS                                 
401800       MOVE ZERO             TO  UT2-DAREGDAT                             
401900       MOVE ZERO             TO  UT2-DASTADAT                             
402000       MOVE ZERO             TO  UT2-PRKURS-SU                            
402100       MOVE ZERO             TO  UT2-PRKURS-SC                            
402200       MOVE ZERO             TO  UT2-PRKURS-UC                            
402300       MOVE ZERO             TO  UT2-PRKURS-CU                            
402310       PERFORM S12-WRITE-W56023A                                          
402400     END-IF                                                               
402500     IF IN2-IDPTYP = 'A18'                                                
402600       MOVE 2A18-IDPTYP      TO  UT2-IDPTYP                               
402700       MOVE 2A18-KDEKOHT     TO  UT2-KDEKOHT                              
402800       MOVE 2A18-IDFTG       TO  UT2-IDFTG                                
402900       MOVE 2A18-IDDC-SEND   TO  UT2-IDDC-SEND                            
403000       MOVE 2A18-IDDC-REC    TO  UT2-IDDC-REC                             
403100       MOVE ZERO             TO  UT2-IDDISTR                              
403200       MOVE ZERO             TO  UT2-IDKUNDNR                             
403300       MOVE ZERO             TO  UT2-IDFAKT                               
403400       MOVE SPACE            TO  UT2-KDFAKTYP                             
403500       MOVE ZERO             TO  UT2-DAFAKT                               
403600       MOVE ZERO             TO  UT2-IDORDNR7                             
403700       MOVE 2A18-IDARTNR     TO  UT2-IDARTNR                              
403800       MOVE 2A18-KDPRODSL    TO  UT2-KDPRODSL                             
403900       MOVE 2A18-KDPSLLOC    TO  UT2-KDPSLLOC                             
404000       MOVE ZERO             TO  UT2-KVLEVART                             
404100       MOVE ZERO             TO  UT2-PRARTNTO                             
404200       MOVE SPACE            TO  UT2-FLOVRLEV                             
404300       MOVE ZERO             TO  UT2-KDFRAKT                              
404400       MOVE ZERO             TO  UT2-SUFAKTRE                             
404500       MOVE ZERO             TO  UT2-PREMBHNT                             
404600       MOVE ZERO             TO  UT2-PRFRAKT                              
404700       MOVE ZERO             TO  UT2-PRFOERS                              
404800       MOVE ZERO             TO  UT2-PRMOMS                               
404900       MOVE ZERO             TO  UT2-PRLEGKST                             
405000       MOVE ZERO             TO  UT2-SUFKTTILL                            
405100       MOVE ZERO             TO  UT2-PRAVDRAG                             
405200       MOVE ZERO             TO  UT2-SUFKTBEL                             
405300       MOVE ZERO             TO  UT2-SUFKTUTL                             
405400       MOVE ZERO             TO  UT2-PRKURS                               
405500       MOVE ZERO             TO  UT2-IDRAPPNR                             
405600       MOVE ZERO             TO  UT2-IDKOLLI                              
405700       MOVE SPACE            TO  UT2-KDANMORS                             
405800       MOVE SPACE            TO  UT2-KDVALISO                             
405900       MOVE 2A18-DAINLINL    TO  UT2-DAINLINL                             
406000       MOVE 2A18-KVANTMOT    TO  UT2-KVANTMOT                             
406100       MOVE ZERO             TO  UT2-KVSKROT                              
406200       MOVE 2A18-PRAVCOST    TO  UT2-PRAVCOST                             
406300       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
406400       MOVE ZERO             TO  UT2-KVLS-OLD                             
406500       MOVE SPACE            TO  UT2-FLSLUT                               
406600       MOVE ZERO             TO  UT2-REMARKUP                             
406700       MOVE ZERO             TO  UT2-IDKNOTNR                             
406800       MOVE ZERO             TO  UT2-DAKRENOT                             
406900       MOVE ZERO             TO  UT2-SUKREUTL                             
407000       MOVE ZERO             TO  UT2-SUKRENTO                             
407100       MOVE ZERO             TO  UT2-PRLANDCO                             
407200       MOVE ZERO             TO  UT2-SUKRENOT                             
407300       MOVE ZERO             TO  UT2-KVKREANT                             
407400       MOVE ZERO             TO  UT2-DARETILL                             
407500       MOVE ZERO             TO  UT2-KVLEVANM                             
407600       MOVE ZERO             TO  UT2-KVRETINL                             
407700       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
407800       MOVE 2A18-IDKONTO     TO  UT2-IDKONTO                              
407900       MOVE SPACE            TO  UT2-IDKST                                
408000       MOVE ZERO             TO  UT2-DAJUSTDA                             
408100       MOVE ZERO             TO  UT2-KVJUSTKV                             
408200       MOVE ZERO             TO  UT2-KDINVKAT                             
408300       MOVE SPACE            TO  UT2-TEINVANM                             
408400       MOVE ZERO             TO  UT2-IDPRODNR                             
408500       MOVE 2A18-IDLEVNR     TO  UT2-IDLEVNR                              
408600       MOVE ZERO             TO  UT2-KVBEART                              
408700       MOVE ZERO             TO  UT2-DAORDDAT                             
408800       MOVE ZERO             TO  UT2-PRARTBEU                             
408900       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
409000       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
409100       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
409200       MOVE 2A18-IDFS        TO  UT2-IDFS                                 
409300       MOVE ZERO             TO  UT2-KVAVIS                               
409400       MOVE ZERO             TO  UT2-IDBYTRAP                             
409500       MOVE ZERO             TO  UT2-KVRETUR                              
409600       MOVE ZERO             TO  UT2-SUAVCOST                             
409700       MOVE SPACE            TO  UT2-IDUSER                               
409800       MOVE SPACE            TO  UT2-KDAVCOST                             
409900       MOVE ZERO             TO UT2-KDPSLLOC-NEW                          
410000       MOVE ZERO             TO UT2-KDPSLLOC-OLD                          
410100       MOVE ZERO             TO  UT2-KVEFRS                               
410200       MOVE ZERO             TO  UT2-KVLS                                 
410300       MOVE ZERO             TO  UT2-DAREGDAT                             
410400       MOVE ZERO             TO  UT2-DASTADAT                             
410500       MOVE ZERO             TO  UT2-PRKURS-SU                            
410600       MOVE ZERO             TO  UT2-PRKURS-SC                            
410700       MOVE ZERO             TO  UT2-PRKURS-UC                            
410800       MOVE ZERO             TO  UT2-PRKURS-CU                            
410810       PERFORM S12-WRITE-W56023A                                          
410900     END-IF                                                               
411000     IF IN2-IDPTYP = 'A19'                                                
411100       MOVE 2A19-IDPTYP      TO  UT2-IDPTYP                               
411200       MOVE 2A19-KDEKOHT     TO  UT2-KDEKOHT                              
411300       MOVE 2A19-IDFTG       TO  UT2-IDFTG                                
411400       MOVE 2A19-IDDC-SEND   TO  UT2-IDDC-SEND                            
411500       MOVE 2A19-IDDC-REC    TO  UT2-IDDC-REC                             
411600       MOVE ZERO             TO  UT2-IDDISTR                              
411700       MOVE ZERO             TO  UT2-IDKUNDNR                             
411800       MOVE ZERO             TO  UT2-IDFAKT                               
411900       MOVE SPACE            TO  UT2-KDFAKTYP                             
412000       MOVE ZERO             TO  UT2-DAFAKT                               
412100       MOVE ZERO             TO  UT2-IDORDNR7                             
412200       MOVE 2A19-IDARTNR     TO  UT2-IDARTNR                              
412300       MOVE 2A19-KDPRODSL    TO  UT2-KDPRODSL                             
412400       MOVE 2A19-KDPSLLOC    TO  UT2-KDPSLLOC                             
412500       MOVE ZERO             TO  UT2-KVLEVART                             
412600       MOVE ZERO             TO  UT2-PRARTNTO                             
412700       MOVE SPACE            TO  UT2-FLOVRLEV                             
412800       MOVE ZERO             TO  UT2-KDFRAKT                              
412900       MOVE ZERO             TO  UT2-SUFAKTRE                             
413000       MOVE ZERO             TO  UT2-PREMBHNT                             
413100       MOVE ZERO             TO  UT2-PRFRAKT                              
413200       MOVE ZERO             TO  UT2-PRFOERS                              
413300       MOVE ZERO             TO  UT2-PRMOMS                               
413400       MOVE ZERO             TO  UT2-PRLEGKST                             
413500       MOVE ZERO             TO  UT2-SUFKTTILL                            
413600       MOVE ZERO             TO  UT2-PRAVDRAG                             
413700       MOVE ZERO             TO  UT2-SUFKTBEL                             
413800       MOVE ZERO             TO  UT2-SUFKTUTL                             
413900       MOVE ZERO             TO  UT2-PRKURS                               
414000       MOVE ZERO             TO  UT2-IDRAPPNR                             
414100       MOVE ZERO             TO  UT2-IDKOLLI                              
414200       MOVE SPACE            TO  UT2-KDANMORS                             
414300       MOVE SPACE            TO  UT2-KDVALISO                             
414400       MOVE ZERO             TO  UT2-DAINLINL                             
414500       MOVE ZERO             TO  UT2-KVANTMOT                             
414600       MOVE ZERO             TO  UT2-KVSKROT                              
414700       MOVE 2A19-PRAVCOST    TO  UT2-PRAVCOST                             
414800       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
414900       MOVE ZERO             TO  UT2-KVLS-OLD                             
415000       MOVE SPACE            TO  UT2-FLSLUT                               
415100       MOVE ZERO             TO  UT2-REMARKUP                             
415200       MOVE ZERO             TO  UT2-IDKNOTNR                             
415300       MOVE ZERO             TO  UT2-DAKRENOT                             
415400       MOVE ZERO             TO  UT2-SUKREUTL                             
415500       MOVE ZERO             TO  UT2-SUKRENTO                             
415600       MOVE ZERO             TO  UT2-PRLANDCO                             
415700       MOVE ZERO             TO  UT2-SUKRENOT                             
415800       MOVE ZERO             TO  UT2-KVKREANT                             
415900       MOVE ZERO             TO  UT2-DARETILL                             
416000       MOVE ZERO             TO  UT2-KVLEVANM                             
416100       MOVE ZERO             TO  UT2-KVRETINL                             
416200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
416300       MOVE ZERO             TO  UT2-IDKONTO                              
416400       MOVE SPACE            TO  UT2-IDKST                                
416500       MOVE ZERO             TO  UT2-DAJUSTDA                             
416600       MOVE 2A19-KVJUSTKV    TO  UT2-KVJUSTKV                             
416700       MOVE ZERO             TO  UT2-KDINVKAT                             
416800       MOVE SPACE            TO  UT2-TEINVANM                             
416900       MOVE ZERO             TO  UT2-IDPRODNR                             
417000       MOVE SPACE            TO  UT2-IDLEVNR                              
417100       MOVE ZERO             TO  UT2-KVBEART                              
417200       MOVE ZERO             TO  UT2-DAORDDAT                             
417300       MOVE ZERO             TO  UT2-PRARTBEU                             
417400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
417500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
417600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
417700       MOVE 2A19-IDFS        TO  UT2-IDFS                                 
417800       MOVE ZERO             TO  UT2-KVAVIS                               
417900       MOVE ZERO             TO  UT2-IDBYTRAP                             
418000       MOVE ZERO             TO  UT2-KVRETUR                              
418100       MOVE ZERO             TO  UT2-SUAVCOST                             
418200       MOVE SPACE            TO  UT2-IDUSER                               
418300       MOVE SPACE            TO  UT2-KDAVCOST                             
418400       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
418500       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
418600       MOVE ZERO             TO  UT2-KVEFRS                               
418700       MOVE ZERO             TO  UT2-KVLS                                 
418800       MOVE 2A19-DAREGDAT    TO  UT2-DAREGDAT                             
418900       MOVE ZERO             TO  UT2-DASTADAT                             
419000       MOVE ZERO             TO  UT2-PRKURS-SU                            
419100       MOVE ZERO             TO  UT2-PRKURS-SC                            
419200       MOVE ZERO             TO  UT2-PRKURS-UC                            
419300       MOVE ZERO             TO  UT2-PRKURS-CU                            
419310       PERFORM S12-WRITE-W56023A                                          
419400     END-IF                                                               
419500     IF IN2-IDPTYP = 'A20'                                                
419600       MOVE 2A20-IDPTYP      TO  UT2-IDPTYP                               
419700       MOVE 2A20-KDEKOHT     TO  UT2-KDEKOHT                              
419800       MOVE 2A20-IDFTG       TO  UT2-IDFTG                                
419900       MOVE 2A20-IDDC-SEND   TO  UT2-IDDC-SEND                            
420000       MOVE 2A20-IDDC-REC    TO  UT2-IDDC-REC                             
420100       MOVE ZERO             TO  UT2-IDDISTR                              
420200       MOVE ZERO             TO  UT2-IDKUNDNR                             
420300       MOVE ZERO             TO  UT2-IDFAKT                               
420400       MOVE SPACE            TO  UT2-KDFAKTYP                             
420500       MOVE ZERO             TO  UT2-DAFAKT                               
420600       MOVE ZERO             TO  UT2-IDORDNR7                             
420700       MOVE ZERO             TO  UT2-IDARTNR                              
420800       MOVE ZERO             TO  UT2-KDPRODSL                             
420900       MOVE ZERO             TO  UT2-KDPSLLOC                             
421000       MOVE ZERO             TO  UT2-KVLEVART                             
421100       MOVE ZERO             TO  UT2-PRARTNTO                             
421200       MOVE SPACE            TO  UT2-FLOVRLEV                             
421300       MOVE ZERO             TO  UT2-KDFRAKT                              
421400       MOVE ZERO             TO  UT2-SUFAKTRE                             
421500       MOVE ZERO             TO  UT2-PREMBHNT                             
421600       MOVE ZERO             TO  UT2-PRFRAKT                              
421700       MOVE ZERO             TO  UT2-PRFOERS                              
421800       MOVE ZERO             TO  UT2-PRMOMS                               
421900       MOVE ZERO             TO  UT2-PRLEGKST                             
422000       MOVE ZERO             TO  UT2-SUFKTTILL                            
422100       MOVE ZERO             TO  UT2-PRAVDRAG                             
422200       MOVE ZERO             TO  UT2-SUFKTBEL                             
422300       MOVE ZERO             TO  UT2-SUFKTUTL                             
422400       MOVE ZERO             TO  UT2-PRKURS                               
422500       MOVE ZERO             TO  UT2-IDRAPPNR                             
422600       MOVE ZERO             TO  UT2-IDKOLLI                              
422700       MOVE SPACE            TO  UT2-KDANMORS                             
422800       MOVE SPACE            TO  UT2-KDVALISO                             
422900       MOVE ZERO             TO  UT2-DAINLINL                             
423000       MOVE ZERO             TO  UT2-KVANTMOT                             
423100       MOVE ZERO             TO  UT2-KVSKROT                              
423200       MOVE ZERO             TO  UT2-PRAVCOST                             
423300       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
423400       MOVE ZERO             TO  UT2-KVLS-OLD                             
423500       MOVE SPACE            TO  UT2-FLSLUT                               
423600       MOVE ZERO             TO  UT2-REMARKUP                             
423700       MOVE ZERO             TO  UT2-IDKNOTNR                             
423800       MOVE ZERO             TO  UT2-DAKRENOT                             
423900       MOVE ZERO             TO  UT2-SUKREUTL                             
424000       MOVE ZERO             TO  UT2-SUKRENTO                             
424100       MOVE ZERO             TO  UT2-PRLANDCO                             
424200       MOVE ZERO             TO  UT2-SUKRENOT                             
424300       MOVE ZERO             TO  UT2-KVKREANT                             
424400       MOVE ZERO             TO  UT2-DARETILL                             
424500       MOVE ZERO             TO  UT2-KVLEVANM                             
424600       MOVE ZERO             TO  UT2-KVRETINL                             
424700       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
424800       MOVE ZERO             TO  UT2-IDKONTO                              
424900       MOVE SPACE            TO  UT2-IDKST                                
425000       MOVE ZERO             TO  UT2-DAJUSTDA                             
425100       MOVE ZERO             TO  UT2-KVJUSTKV                             
425200       MOVE ZERO             TO  UT2-KDINVKAT                             
425300       MOVE SPACE            TO  UT2-TEINVANM                             
425400       MOVE ZERO             TO  UT2-IDPRODNR                             
425500       MOVE SPACE            TO  UT2-IDLEVNR                              
425600       MOVE ZERO             TO  UT2-KVBEART                              
425700       MOVE ZERO             TO  UT2-DAORDDAT                             
425800       MOVE ZERO             TO  UT2-PRARTBEU                             
425900       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
426000       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
426100       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
426200       MOVE SPACE            TO  UT2-IDFS                                 
426300       MOVE ZERO             TO  UT2-KVAVIS                               
426400       MOVE ZERO             TO  UT2-IDBYTRAP                             
426500       MOVE ZERO             TO  UT2-KVRETUR                              
426600       MOVE ZERO             TO  UT2-SUAVCOST                             
426700       MOVE SPACE            TO  UT2-IDUSER                               
426800       MOVE SPACE            TO  UT2-KDAVCOST                             
426900       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
427000       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
427100       MOVE ZERO             TO  UT2-KVEFRS                               
427200       MOVE ZERO             TO  UT2-KVLS                                 
427300       MOVE ZERO             TO  UT2-DAREGDAT                             
427400       MOVE 2A20-DASTADAT    TO  UT2-DASTADAT                             
427500       MOVE 2A20-PRKURS-SU   TO  UT2-PRKURS-SU                            
427600       MOVE 2A20-PRKURS-SC   TO  UT2-PRKURS-SC                            
427700       MOVE 2A20-PRKURS-UC   TO  UT2-PRKURS-UC                            
427800       MOVE 2A20-PRKURS-CU   TO  UT2-PRKURS-CU                            
427810       PERFORM S12-WRITE-W56023A                                          
427900     END-IF                                                               
428000     IF IN2-IDPTYP = 'L09'                                                
428100       MOVE 2L09-IDPTYP      TO  UT2-IDPTYP                               
428200       MOVE 2L09-KDEKOHT     TO  UT2-KDEKOHT                              
428300       MOVE 2L09-IDFTG       TO  UT2-IDFTG                                
428400       MOVE 2L09-IDDC-SEND   TO  UT2-IDDC-SEND                            
428500       MOVE 2L09-IDDC-REC    TO  UT2-IDDC-REC                             
428600       MOVE 2L09-IDDISTR     TO  UT2-IDDISTR                              
428700       MOVE 2L09-IDKUNDNR    TO  UT2-IDKUNDNR                             
428800       MOVE 2L09-IDFAKT      TO  UT2-IDFAKT                               
428900       MOVE SPACE            TO  UT2-KDFAKTYP                             
429000       MOVE 2L09-DAFAKT      TO  UT2-DAFAKT                               
429100       MOVE 2L09-IDORDNR7    TO  UT2-IDORDNR7                             
429200       MOVE 2L09-IDARTNR     TO  UT2-IDARTNR                              
429300       MOVE 2L09-KDPRODSL    TO  UT2-KDPRODSL                             
429400       MOVE 2L09-KDPSLLOC    TO  UT2-KDPSLLOC                             
429500       MOVE 2L09-KVLEVART    TO  UT2-KVLEVART                             
429600       MOVE ZERO             TO  UT2-PRARTNTO                             
429700       MOVE 2L09-FLOVRLEV    TO  UT2-FLOVRLEV                             
429800       MOVE ZERO             TO  UT2-KDFRAKT                              
429900       MOVE ZERO             TO  UT2-SUFAKTRE                             
430000       MOVE ZERO             TO  UT2-PREMBHNT                             
430100       MOVE ZERO             TO  UT2-PRFRAKT                              
430200       MOVE ZERO             TO  UT2-PRFOERS                              
430300       MOVE ZERO             TO  UT2-PRMOMS                               
430400       MOVE ZERO             TO  UT2-PRLEGKST                             
430500       MOVE ZERO             TO  UT2-SUFKTTILL                            
430600       MOVE ZERO             TO  UT2-PRAVDRAG                             
430700       MOVE ZERO             TO  UT2-SUFKTBEL                             
430800       MOVE ZERO             TO  UT2-SUFKTUTL                             
430900       MOVE ZERO             TO  UT2-PRKURS                               
431000       MOVE ZERO             TO  UT2-IDRAPPNR                             
431100       MOVE ZERO             TO  UT2-IDKOLLI                              
431200       MOVE SPACE            TO  UT2-KDANMORS                             
431300       MOVE SPACE            TO  UT2-KDVALISO                             
431400       MOVE ZERO             TO  UT2-DAINLINL                             
431500       MOVE ZERO             TO  UT2-KVANTMOT                             
431600       MOVE ZERO             TO  UT2-KVSKROT                              
431700       MOVE 2L09-PRAVCOST    TO  UT2-PRAVCOST                             
431800       MOVE ZERO             TO  UT2-PRAVCOST-OLD                         
431900       MOVE ZERO             TO  UT2-KVLS-OLD                             
432000       MOVE SPACE            TO  UT2-FLSLUT                               
432100       MOVE ZERO             TO  UT2-REMARKUP                             
432200       MOVE ZERO             TO  UT2-IDKNOTNR                             
432300       MOVE ZERO             TO  UT2-DAKRENOT                             
432400       MOVE ZERO             TO  UT2-SUKREUTL                             
432500       MOVE ZERO             TO  UT2-SUKRENTO                             
432600       MOVE ZERO             TO  UT2-PRLANDCO                             
432700       MOVE ZERO             TO  UT2-SUKRENOT                             
432800       MOVE ZERO             TO  UT2-KVKREANT                             
432900       MOVE ZERO             TO  UT2-DARETILL                             
433000       MOVE ZERO             TO  UT2-KVLEVANM                             
433100       MOVE ZERO             TO  UT2-KVRETINL                             
433200       MOVE ZERO             TO  UT2-KVRETINL-SKR                         
433300       MOVE ZERO             TO  UT2-IDKONTO                              
433400       MOVE SPACE            TO  UT2-IDKST                                
433500       MOVE ZERO             TO  UT2-DAJUSTDA                             
433600       MOVE ZERO             TO  UT2-KVJUSTKV                             
433700       MOVE ZERO             TO  UT2-KDINVKAT                             
433800       MOVE SPACE            TO  UT2-TEINVANM                             
433900       MOVE ZERO             TO  UT2-IDPRODNR                             
434000       MOVE SPACE            TO  UT2-IDLEVNR                              
434100       MOVE ZERO             TO  UT2-KVBEART                              
434200       MOVE ZERO             TO  UT2-DAORDDAT                             
434300       MOVE ZERO             TO  UT2-PRARTBEU                             
434400       MOVE ZERO             TO  UT2-PRARTNTO-GNB                         
434500       MOVE SPACE            TO  UT2-IDFAKT-GNB                           
434600       MOVE ZERO             TO  UT2-DAFAKT-GNB                           
434700       MOVE SPACE            TO  UT2-IDFS                                 
434800       MOVE ZERO             TO  UT2-KVAVIS                               
434900       MOVE ZERO             TO  UT2-IDBYTRAP                             
435000       MOVE ZERO             TO  UT2-KVRETUR                              
435100       MOVE ZERO             TO  UT2-SUAVCOST                             
435200       MOVE SPACE            TO  UT2-IDUSER                               
435300       MOVE SPACE            TO  UT2-KDAVCOST                             
435400       MOVE ZERO             TO  UT2-KDPSLLOC-NEW                         
435500       MOVE ZERO             TO  UT2-KDPSLLOC-OLD                         
435600       MOVE ZERO             TO  UT2-KVEFRS                               
435700       MOVE ZERO             TO  UT2-KVLS                                 
435800       MOVE ZERO             TO  UT2-DAREGDAT                             
435900       MOVE ZERO             TO  UT2-DASTADAT                             
436000       MOVE ZERO             TO  UT2-PRKURS-SU                            
436100       MOVE ZERO             TO  UT2-PRKURS-SC                            
436200       MOVE ZERO             TO  UT2-PRKURS-UC                            
436300       MOVE ZERO             TO  UT2-PRKURS-CU                            
436310       PERFORM S12-WRITE-W56023A                                          
436400     END-IF                                                               
436500     MOVE SPACE             TO UT2-HEADER                                 
436600     .                                                                    
436700     EJECT                                                                
436800 Z-FINIT SECTION.                                                         
436900     CLOSE W56022                                                         
437000           W56023                                                         
437100           W56022A                                                        
437200           W56023A                                                        
437300     SKIP2                                                                
437400     MOVE 'S' TO POSTSUM-OPKOD                                            
437500     CALL POSTSUM USING POSTSUM-PARM                                      
437600     .                                                                    
437700     EJECT                                                                
437800 S01-READ-W56022  SECTION.                                                
437900     READ W56022 INTO IN1-AREA                                            
438000      AT END MOVE YES TO W56022-EOF-SW                                    
438100     END-READ                                                             
439100     .                                                                    
439200     EJECT                                                                
439300 S02-READ-W56023  SECTION.                                                
439310     READ W56023 INTO IN2-AREA                                            
439320      AT END MOVE YES TO W56023-EOF-SW                                    
439330     END-READ                                                             
440600     .                                                                    
440700     EJECT                                                                
440710 S11-WRITE-HEADERS SECTION.                                               
440720                                                                          
440730     WRITE LISTPOST1           FROM UT1-HEADER                            
440740                                                                          
440750     WRITE LISTPOST2           FROM UT2-HEADER                            
440760     .                                                                    
440770                                                                          
440800 S11-WRITE-W56022A SECTION.                                               
440900                                                                          
441000     WRITE LISTPOST1 FROM UT1-AREA                                        
441100                                                                          
441600     .                                                                    
441700     EJECT                                                                
441800 S12-WRITE-W56023A SECTION.                                               
441900                                                                          
442000     WRITE LISTPOST2 FROM UT2-AREA                                        
442100                                                                          
442600     .                                                                    
442700     EJECT                                                                
442800 S99-ABEND SECTION.                                                       
442900                                                                          
443000     SKIP2                                                                
443100     MOVE 'S' TO POSTSUM-OPKOD                                            
443200     CALL POSTSUM USING POSTSUM-PARM                                      
443300     CALL ABEND USING RKOD-ABEND                                          
443400     .                                                                    
