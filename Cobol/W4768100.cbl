000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4768100.                                                
000300 AUTHOR.         CAMELIA OLGRENER.                                        
000400 DATE-WRITTEN.   03/04/08.                                                
000500 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKAPAR FIL FÖR ÖVERFÖRING AV FAKTURAINFORMATION                  
001000*        TILL TULLSYSTEM                                                  
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700*    ÄNDRAT FEBR. 2019 FÖR DET NYA TULLSYST. MIC                          
001800*    * VÅR FIL GÅR TILL SYST. FLS SOM SKICKAR VIDARE TILL MIC             
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- FAKTURAPOSTER                                              
002900     SELECT W47680                     ASSIGN TO W47681D1.                
003000     SKIP2                                                                
003100*          --- FIL FÖR ÖVERSÄTTNING TILL EDI-FORMAT                       
003200     SELECT W47681                     ASSIGN TO W47681D2.                
003300     SKIP2                                                                
003400*          --- FIL FÖR ÖVERSÄTTNING TILL FLS TO MIC                       
003500     SELECT W47681X                    ASSIGN TO W47681D3.                
003600     SKIP2                                                                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W47680                                                               
004300     RECORDING       V                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600 01  INPOST.                                                              
004700*    03  -COPY W476TU1 -L.                                                
004800*                                                                         
004900     EJECT                                                                
005000 FD  W47681                                                               
005100     RECORDING       V                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400 01  UT-S01-POST.                                                         
005500*    03  -COPY WEDIS01  -PRE UT-                                          
005600                                                                          
005700 01  UT-G01-POST.                                                         
005800*    03  -COPY WEDIG01  -PRE UT-                                          
005900                                                                          
006000 01  UT-G02-POST.                                                         
006100*    03  -COPY WEDIG02  -PRE UT-                                          
006200                                                                          
006300 01  UT-H01-POST.                                                         
006400*    03  -COPY WEDIH01  -PRE UT-                                          
006500                                                                          
006600 01  UT-H02-POST.                                                         
006700*    03  -COPY WEDIH02  -PRE UT-                                          
006800                                                                          
006900 01  UT-H03-POST.                                                         
007000*    03  -COPY WEDIH03  -PRE UT-.                                         
007100                                                                          
007200 01  UT-H04-POST.                                                         
007300*    03  -COPY WEDIH04  -PRE UT-.                                         
007400                                                                          
007500 01  UT-H11-POST.                                                         
007600*    03  -COPY WEDIH11  -PRE UT-.                                         
007700                                                                          
007800 01  UT-H13-POST.                                                         
007900*    03  -COPY WEDIH13  -PRE UT-.                                         
008000                                                                          
008100 01  UT-H14-POST.                                                         
008200*    03  -COPY WEDIH14  -PRE UT-.                                         
008300                                                                          
008400 01  UT-H17-POST.                                                         
008500*    03  -COPY WEDIH17  -PRE UT-.                                         
008600                                                                          
008700 01  UT-H18-POST.                                                         
008800*    03  -COPY WEDIH18  -PRE UT-.                                         
008900                                                                          
009000 01  UT-H24-POST.                                                         
009100*    03  -COPY WEDIH24  -PRE UT-.                                         
009200                                                                          
009300 01  UT-H26-POST.                                                         
009400*    03  -COPY WEDIH26  -PRE UT-.                                         
009500                                                                          
009600 01  UT-H27-POST.                                                         
009700*    03  -COPY WEDIH27  -PRE UT-.                                         
009800                                                                          
009900 01  UT-A01-POST.                                                         
010000*    03  -COPY WEDIA01  -PRE UT-                                          
010100                                                                          
010200 01  UT-A02-POST.                                                         
010300*    03  -COPY WEDIA02  -PRE UT-                                          
010400                                                                          
010500 01  UT-A03-POST.                                                         
010600*    03  -COPY WEDIA03  -PRE UT-.                                         
010700                                                                          
010800 01  UT-A04-POST.                                                         
010900*    03  -COPY WEDIA04  -PRE UT-.                                         
011000                                                                          
011100 01  UT-A06-POST.                                                         
011200*    03  -COPY WEDIA06  -PRE UT-.                                         
011300                                                                          
011400 01  UT-P01-POST.                                                         
011500*    03  -COPY WEDIP01  -PRE UT-.                                         
011600                                                                          
011700 01  UT-P05-POST.                                                         
011800*    03  -COPY WEDIP05  -PRE UT-.                                         
011900                                                                          
012000 01  UT-P08-POST.                                                         
012100*    03  -COPY WEDIP08  -PRE UT-.                                         
012200                                                                          
012300 01  UT-E01-POST.                                                         
012400*    03  -COPY WEDIE01 -PRE UT-.                                          
012500                                                                          
012600**-- NY FIL TILL TULLEN                                                   
012700 FD  W47681X                                                              
012800     RECORDING       V                                                    
012900     BLOCK CONTAINS  0.                                                   
013000                                                                          
013100 01  UT2-S01-POST.                                                        
013200*    03  -COPY WEDIS01  -PRE UT2-                                         
013300                                                                          
013400 01  UT2-G01-POST.                                                        
013500*    03  -COPY WEDIG01  -PRE UT2-                                         
013600                                                                          
013700 01  UT2-G02-POST.                                                        
013800*    03  -COPY WEDIG02  -PRE UT2-                                         
013900                                                                          
014000 01  UT2-H01-POST.                                                        
014100*    03  -COPY WEDIH01  -PRE UT2-                                         
014200                                                                          
014300 01  UT2-H02-POST.                                                        
014400*    03  -COPY WEDIH02  -PRE UT2-                                         
014500                                                                          
014600 01  UT2-H03-POST.                                                        
014700*    03  -COPY WEDIH03  -PRE UT2-.                                        
014800                                                                          
014900 01  UT2-H04-POST.                                                        
015000*    03  -COPY WEDIH04  -PRE UT2-.                                        
015100                                                                          
015200 01  UT2-H11-POST.                                                        
015300*    03  -COPY WEDIH11  -PRE UT2-.                                        
015400                                                                          
015500 01  UT2-H13-POST.                                                        
015600*    03  -COPY WEDIH13  -PRE UT2-.                                        
015700                                                                          
015800 01  UT2-H14-POST.                                                        
015900*    03  -COPY WEDIH14  -PRE UT2-.                                        
016000                                                                          
016100 01  UT2-H17-POST.                                                        
016200*    03  -COPY WEDIH17  -PRE UT2-.                                        
016300                                                                          
016400 01  UT2-H18-POST.                                                        
016500*    03  -COPY WEDIH18  -PRE UT2-.                                        
016600                                                                          
016700 01  UT2-H24-POST.                                                        
016800*    03  -COPY WEDIH24  -PRE UT2-.                                        
016900                                                                          
017000 01  UT2-H26-POST.                                                        
017100*    03  -COPY WEDIH26  -PRE UT2-.                                        
017200                                                                          
017300 01  UT2-H27-POST.                                                        
017400*    03  -COPY WEDIH27  -PRE UT2-.                                        
017500                                                                          
017600 01  UT2-A01-POST.                                                        
017700*    03  -COPY WEDIA01  -PRE UT2-                                         
017800                                                                          
017900 01  UT2-A02-POST.                                                        
018000*    03  -COPY WEDIA02  -PRE UT2-                                         
018100                                                                          
018200 01  UT2-A03-POST.                                                        
018300*    03  -COPY WEDIA03  -PRE UT2-.                                        
018400                                                                          
018500 01  UT2-A04-POST.                                                        
018600*    03  -COPY WEDIA04  -PRE UT2-.                                        
018700                                                                          
018800 01  UT2-A06-POST.                                                        
018900*    03  -COPY WEDIA06  -PRE UT2-.                                        
019000                                                                          
019100 01  UT2-P01-POST.                                                        
019200*    03  -COPY WEDIP01  -PRE UT2-.                                        
019300                                                                          
019400 01  UT2-P05-POST.                                                        
019500*    03  -COPY WEDIP05  -PRE UT2-.                                        
019600                                                                          
019700 01  UT2-P08-POST.                                                        
019800*    03  -COPY WEDIP08  -PRE UT2-.                                        
019900                                                                          
020000 01  UT2-E01-POST.                                                        
020100*    03  -COPY WEDIE01 -PRE UT2-.                                         
020200                                                                          
020300     EJECT                                                                
020400 WORKING-STORAGE SECTION.                                                 
020500                                                                          
020600                                                                          
020700*    -- CHECKED BY WY2000                                                 
020800 77  IDPGM                       PIC X(8)    VALUE 'W4768100'.            
020900 77  JA                          PIC X       VALUE 'J'.                   
021000 77  YES                         PIC X       VALUE 'Y'.                   
021100 77  NEJ                         PIC X       VALUE 'N'.                   
021200 77  WS-KOLLI-RAK                PIC 9(4)    VALUE ZERO.                  
021300 77  WS-IDDISTR                  PIC S9(5)   VALUE ZERO.                  
021400 77  WS-BESTLAND                 PIC X(2)    VALUE SPACE.                 
021500 77  WS-PUNKT                    PIC X(1)    VALUE '.'.                   
021600 77  WS-STRECK                   PIC X(1)    VALUE '/'.                   
021700 77  WS-LINJE                    PIC X(1)    VALUE '_'.                   
021800 77  WS-TVA                      PIC X(1)    VALUE '2'.                   
021900 77  WS-TJUGO                    PIC X(2)    VALUE '20'.                  
022000 77  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO.                  
022100 77  WS-A01-LINE                 PIC 9(6)    VALUE ZERO.                  
022200 77  WS-KDTRPSATT                PIC 9(1)    VALUE ZERO.                  
022300                                                                          
022400 77  W47680-EOF-SW               PIC X       VALUE 'N'.                   
022500     88  W47680-EOF                          VALUE 'J'.                   
022600                                                                          
022700     EJECT                                                                
022800 01  FILLER                      PIC X(16)  VALUE 'WS-SEKTION'.           
022900 01  WS-SEKTION                  PIC X(30)  VALUE SPACE.                  
023000                                                                          
023100 01  ARBETSFALT.                                                          
023200                                                                          
023300     03 WS-NUMBER-OF-SEGMENTS       PIC 9(06) VALUE ZERO.                 
023400                                                                          
023500     03 IN-IDPTYP                   PIC X(03) VALUE SPACE.                
023600     03 EDI-IDPTYP                  PIC X(03) VALUE SPACE.                
023700                                                                          
023800 01  WS-IDSTATNR                 PIC X(9).                                
023900 01  WS-IDSTATNR-TILLAGG.                                                 
024000     03 WS-IDSTATNR-ENBART       PIC X(8).                                
024100     03 WS-BLANKT                PIC X(4) VALUE SPACE.                    
024200     03 WS-TILLAGG               PIC X(4).                                
024300                                                                          
024400 01  WS-SUARTNTO-NUM             PIC 9(9)V9(2).                           
024500 01  FILLER REDEFINES WS-SUARTNTO-NUM.                                    
024600     03  WS-SUARTNTO-HEL     PIC X(9).                                    
024700     03  WS-SUARTNTO-DEC     PIC X(2).                                    
024800                                                                          
024900 01  WS-SUARTNTO-PUNKT           PIC X(12).                               
025000 01  FILLER REDEFINES WS-SUARTNTO-PUNKT.                                  
025100     03  WS-S-HEL                PIC X(9).                                
025200     03  WS-S-PUNKT              PIC X(1).                                
025300     03  WS-S-DEC                PIC X(2).                                
025400                                                                          
025500 01  WS-PRARTNTO-NUM             PIC 9(7)V9(2).                           
025600 01  FILLER REDEFINES WS-PRARTNTO-NUM.                                    
025700     03  WS-PRARTNTO-HEL         PIC X(7).                                
025800     03  WS-PRARTNTO-DEC         PIC X(2).                                
025900                                                                          
026000 01  WS-PRARTNTO-PUNKT           PIC X(10).                               
026100 01  FILLER REDEFINES WS-PRARTNTO-PUNKT.                                  
026200     03  WS-P-HEL                PIC X(7).                                
026300     03  WS-P-PUNKT              PIC X(1).                                
026400     03  WS-P-DEC                PIC X(2).                                
026500                                                                          
026600 01  WS-VKRADNTO-NUM             PIC 9(6)V9(3).                           
026700 01  FILLER REDEFINES WS-VKRADNTO-NUM.                                    
026800     03  WS-VKRADNTO-HEL         PIC X(6).                                
026900     03  WS-VKRADNTO-DEC         PIC X(3).                                
027000                                                                          
027100 01  WS-VKRADNTO-PUNKT           PIC X(10).                               
027200 01  FILLER REDEFINES WS-VKRADNTO-PUNKT.                                  
027300     03  WS-V-HEL                PIC X(6).                                
027400     03  WS-V-PUNKT              PIC X(1).                                
027500     03  WS-V-DEC                PIC X(3).                                
027600                                                                          
027700 01  WS-VKORDBTO-NUM             PIC 9(6)V9(1).                           
027800 01  FILLER REDEFINES WS-VKORDBTO-NUM.                                    
027900     03  WS-VKORDBTO-HEL         PIC X(6).                                
028000     03  WS-VKORDBTO-DEC         PIC X(1).                                
028100                                                                          
028200 01  WS-VKORDBTO-PUNKT           PIC X(08).                               
028300 01  FILLER REDEFINES WS-VKORDBTO-PUNKT.                                  
028400     03  WS-B-HEL                PIC X(6).                                
028500     03  WS-B-PUNKT              PIC X(1).                                
028600     03  WS-B-DEC                PIC X(1).                                
028700                                                                          
028800 01  WS-VLORDBTO-NUM             PIC 9(4)V9(3).                           
028900 01  FILLER REDEFINES WS-VLORDBTO-NUM.                                    
029000     03  WS-VLORDBTO-HEL         PIC X(4).                                
029100     03  WS-VLORDBTO-DEC         PIC X(3).                                
029200                                                                          
029300 01  WS-VLORDBTO-PUNKT           PIC X(08).                               
029400 01  FILLER REDEFINES WS-VLORDBTO-PUNKT.                                  
029500     03  WS-L-HEL                PIC X(4).                                
029600     03  WS-L-PUNKT              PIC X(1).                                
029700     03  WS-L-DEC                PIC X(3).                                
029800                                                                          
029900 01  WS-TIFAKT                   PIC X(07).                               
030000 01  FILLER REDEFINES WS-TIFAKT.                                          
030100     03  WS-T-AR                 PIC X(3).                                
030200     03  WS-T-RESTEN             PIC X(4).                                
030300                                                                          
030400 01  WS-TIFAKT-HEL               PIC X(08).                               
030500 01  FILLER REDEFINES WS-TIFAKT-HEL.                                      
030600     03  WS-TIFAKT-A1            PIC X(1).                                
030700     03  WS-TIFAKT-A2            PIC X(3).                                
030800     03  WS-TIFAKT-RESTEN        PIC X(4).                                
030900                                                                          
031000 01  WS-DARFS                    PIC X(12).                               
031100 01  FILLER REDEFINES WS-DARFS.                                           
031200     03  WS-DARFS-CHA            PIC X(12).                               
031300                                                                          
031400 01  WS-ADDITIONAL-NUM           PIC 9(7)V9(2).                           
031500 01  FILLER REDEFINES WS-ADDITIONAL-NUM.                                  
031600     03  WS-ADDITIONAL-HEL       PIC X(7).                                
031700     03  WS-ADDITIONAL-DEC       PIC X(2).                                
031800                                                                          
031900 01  WS-ADDITIONAL-PUNKT         PIC X(10).                               
032000 01  FILLER REDEFINES WS-ADDITIONAL-PUNKT.                                
032100     03  WS-A-HEL                PIC X(7).                                
032200     03  WS-A-PUNKT              PIC X(1).                                
032300     03  WS-A-DEC                PIC X(2).                                
032400                                                                          
032500 01  KONSTANTER.                                                          
032600                                                                          
032700     03 S01-UPPGIFTER.                                                    
032800       05 WC-S01-IDPTYP                 PIC X(03) VALUE 'S01'.            
032900       05 WC-S01-G                      PIC X(01) VALUE 'G'.              
033000       05 WC-S01-G00                    PIC X(02) VALUE '00'.             
033100       05 WC-S01-G02                    PIC X(02) VALUE '02'.             
033200       05 WC-S01-H                      PIC X(01) VALUE 'H'.              
033300       05 WC-S01-H27                    PIC X(02) VALUE '27'.             
033400       05 WC-S01-A                      PIC X(01) VALUE 'A'.              
033500       05 WC-S01-A06                    PIC X(02) VALUE '06'.             
033600       05 WC-S01-P                      PIC X(01) VALUE 'P'.              
033700       05 WC-S01-P08                    PIC X(02) VALUE '08'.             
033800       05 WC-S01-D                      PIC X(01) VALUE 'D'.              
033900       05 WC-S01-D02                    PIC X(02) VALUE '02'.             
034000       05 WC-S01-N                      PIC X(01) VALUE 'N'.              
034100       05 WC-S01-N00                    PIC X(02) VALUE '00'.             
034200       05 WC-S01-E                      PIC X(01) VALUE 'E'.              
034300       05 WC-S01-E01                    PIC X(02) VALUE '01'.             
034400                                                                          
034500     03 G01-UPPGIFTER.                                                    
034600       05 WC-G01-IDPTYP                 PIC X(03) VALUE 'G01'.            
034700       05 WC-G01-COMPANY-CODE           PIC X(04) VALUE 'VCCS'.           
034800       05 WC-G01-ETT                    PIC X(01) VALUE '1'.              
034900       05 WC-G01-ZERO                   PIC X(01) VALUE '0'.              
035000                                                                          
035100     03 G02-UPPGIFTER.                                                    
035200       05 WC-G02-IDPTYP                 PIC X(03) VALUE 'G02'.            
035300       05 WC-G02-INPUT-TYPE             PIC X(01) VALUE 'F'.              
035400                                                                          
035500     03 H01-UPPGIFTER.                                                    
035600       05 WC-H01-IDPTYP                 PIC X(03) VALUE 'H01'.            
035700       05 WC-H01-INPUT-TYPE             PIC X(01) VALUE 'F'.              
035800                                                                          
035900     03 H02-UPPGIFTER.                                                    
036000       05 WC-H02-IDPTYP                 PIC X(03) VALUE 'H02'.            
036100                                                                          
036200     03 H03-UPPGIFTER.                                                    
036300       05 WC-H03-IDPTYP                 PIC X(03) VALUE 'H03'.            
036400                                                                          
036500     03 H04-UPPGIFTER.                                                    
036600       05 WC-H04-IDPTYP                 PIC X(03) VALUE 'H04'.            
036700                                                                          
036800     03 H11-UPPGIFTER.                                                    
036900       05 WC-H11-IDPTYP                 PIC X(03) VALUE 'H11'.            
037000                                                                          
037100     03 H13-UPPGIFTER.                                                    
037200       05 WC-H13-IDPTYP                 PIC X(03) VALUE 'H13'.            
037300                                                                          
037400     03 H14-UPPGIFTER.                                                    
037500       05 WC-H14-IDPTYP                 PIC X(03) VALUE 'H14'.            
037600                                                                          
037700     03 H17-UPPGIFTER.                                                    
037800       05 WC-H17-IDPTYP                 PIC X(03) VALUE 'H17'.            
037900       05 WC-H17-ORIGIN-COUNTRY         PIC X(02) VALUE 'SE'.             
038000       05 WC-H17-ORIGIN-COUNTRY-2       PIC X(02) VALUE 'ES'.             
038100       05 WC-H17-ORIGIN-COUNTRY-3       PIC X(02) VALUE 'AE'.             
038200       05 WC-H17-ORIGIN-COUNTRY-4       PIC X(02) VALUE 'NL'.             
038300       05 WC-H17-ORIGIN-COUNTRY-5       PIC X(02) VALUE 'US'.             
038400       05 WC-H17-ORIGIN-COUNTRY-6       PIC X(02) VALUE 'CN'.             
038500                                                                          
038600     03 H18-UPPGIFTER.                                                    
038700       05 WC-H18-IDPTYP                 PIC X(03) VALUE 'H18'.            
038800       05 WC-H18-ISO-SE                 PIC X(02) VALUE 'SE'.             
038900       05 WC-H18-ISO-ES                 PIC X(02) VALUE 'ES'.             
039000       05 WC-H18-ISO-AE                 PIC X(02) VALUE 'AE'.             
039100       05 WC-H18-ISO-NL                 PIC X(02) VALUE 'NL'.             
039200       05 WC-H18-ISO-US                 PIC X(02) VALUE 'US'.             
039300       05 WC-H18-ISO-CN                 PIC X(02) VALUE 'CN'.             
039400                                                                          
039500     03 H24-UPPGIFTER.                                                    
039600       05 WC-H24-IDPTYP                 PIC X(03) VALUE 'H24'.            
039700                                                                          
039800     03 H26-UPPGIFTER.                                                    
039900       05 WC-H26-IDPTYP                 PIC X(03) VALUE 'H26'.            
040000                                                                          
040100     03 H27-UPPGIFTER.                                                    
040200       05 WC-H27-IDPTYP                 PIC X(03) VALUE 'H27'.            
040300                                                                          
040400     03 A01-UPPGIFTER.                                                    
040500       05 WC-A01-IDPTYP                 PIC X(03) VALUE 'A01'.            
040600       05 WC-A01-BILDELAR               PIC X(08) VALUE                   
040700                                                  'BILDELAR'.             
040800                                                                          
040900     03 A02-UPPGIFTER.                                                    
041000       05 WC-A02-IDPTYP                 PIC X(03) VALUE 'A02'.            
041100       05 WC-A02-TYPE-ART               PIC X(02) VALUE 'AA'.             
041200       05 WC-A02-ETT                    PIC X(01) VALUE '1'.              
041300                                                                          
041400     03 A03-UPPGIFTER.                                                    
041500       05 WC-A03-IDPTYP                 PIC X(03) VALUE 'A03'.            
041600       05 WC-A03-4039                   PIC X(04) VALUE '4039'.           
041700                                                                          
041800     03 A04-UPPGIFTER.                                                    
041900       05 WC-A04-IDPTYP                 PIC X(03) VALUE 'A04'.            
042000       05 WC-A04-OTHER-Q-1              PIC 9(01) VALUE 1.                
042100       05 WC-A04-OTHER-Q-0              PIC 9(01) VALUE 0.                
042200                                                                          
042300     03 A06-UPPGIFTER.                                                    
042400       05 WC-A06-IDPTYP                 PIC X(03) VALUE 'A06'.            
042500       05 WC-A06-PROD-1041              PIC X(04) VALUE '1041'.           
042600       05 WC-A06-PROD-1000              PIC X(04) VALUE '1000'.           
042700                                                                          
042800     03 P01-UPPGIFTER.                                                    
042900       05 WC-P01-IDPTYP                 PIC X(03) VALUE 'P01'.            
043000       05 WC-P01-TYPE-OF-PACKAGES       PIC X(05) VALUE 'KOLLI'.          
043100                                                                          
043200     03 P05-UPPGIFTER.                                                    
043300       05 WC-P05-IDPTYP                 PIC X(03) VALUE 'P05'.            
043400                                                                          
043500     03 P08-UPPGIFTER.                                                    
043600       05 WC-P08-IDPTYP                 PIC X(03) VALUE 'P08'.            
043700                                                                          
043800     03 E01-UPPGIFTER.                                                    
043900       05 WC-E01-IDPTYP                 PIC X(03) VALUE 'E01'.            
044000                                                                          
044100                                                                          
044200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
044300 01  FILLER REDEFINES DAGENS-DATUM.                                       
044400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
044500     03  DAGENS-DATUM-MM         PIC 9(2).                                
044600     03  DAGENS-DATUM-DD         PIC 9(2).                                
044700                                                                          
044800 01  WS-DATUM-HEL                PIC X(05).                               
044900 01  FILLER REDEFINES WS-DATUM-HEL.                                       
045000     03  WS-DATUM-0              PIC X(1).                                
045100     03  WS-DATUM-AA1            PIC X(2).                                
045200     03  WS-DATUM-AA2            PIC X(2).                                
045300                                                                          
045400 01  DAGENS-TIME                 PIC 9(8).                                
045500 01  FILLER REDEFINES DAGENS-TIME.                                        
045600     03 WS-CURRENT-HHMM          PIC 9(4).                                
045700     03 WS-CURRENT-SSSS          PIC 9(4).                                
045800                                                                          
045900 01  WS-DATUM-TID                PIC X(12).                               
046000 01  FILLER REDEFINES WS-DATUM-TID.                                       
046100     03  WS-DAT-AA1              PIC X(2).                                
046200     03  WS-DAT-AA2              PIC X(2).                                
046300     03  WS-DAT-MM               PIC X(2).                                
046400     03  WS-DAT-DD               PIC X(2).                                
046500     03  WS-DAT-HHMM             PIC X(4).                                
046600                                                                          
046700     EJECT                                                                
046800                                                                          
046900*- - - - - - - - - - - - - -                                              
047000*      --- VALID IDDC CODES                                               
047100*                                                                         
047200*01    -COPY WWDCKONS                                                     
047300     EJECT                                                                
047400*01    -COPY WWDC99                                                       
047500     EJECT                                                                
047600 01  TEST-IDDISTR       PIC 9(5)   COMP-3.                                
047700                                                                          
047800*01  FILLER -COPY WWDIST03    -RED TEST-IDDISTR.                          
047900     EJECT                                                                
048000*01  FILLER -COPY WWDIST42    -RED TEST-IDDISTR.                          
048100     EJECT                                                                
048200*01  FILLER -COPY WWDIST79    -RED TEST-IDDISTR.                          
048300                                                                          
048400 01  W475STAT-START         PIC X(16)   VALUE   'W475STAT-START'.         
048500*                                                                         
048600*01  -COPY W475STAT                                                       
048700     EJECT                                                                
048800 01  W475BEST-START         PIC X(16)   VALUE   'W475BEST-START'.         
048900*                                                                         
049000*01  -COPY W475BEST                                                       
049100     EJECT                                                                
049200 01  FILLER                 PIC X(16)   VALUE 'OMRÄKNA VALUTA'.           
049300*    -COPY W411EXCH                                                       
049400     EJECT                                                                
049500 01  DYNAMISKA-SUBPROGRAM.                                                
049600*                                                                         
049700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
049800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
049900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
050000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
050100     03  W475BEST                PIC X(8)    VALUE 'W475BEST'.            
050200     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
050300     SKIP2                                                                
050400*    --- PARAMETRAR TILL ABEND                                            
050500                                                                          
050600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
050700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
050800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
050900     SKIP2                                                                
051000 01  FELTEXT.                                                             
051100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
051200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
051300     EJECT                                                                
051400*    --- PARAMETRAR TILL POSTSUM                                          
051500*                                                                         
051600*01  -COPY W0005   -PRE  POSTSUM-                                         
051700     EJECT                                                                
051800                                                                          
051900 01  IN-AREA-START             PIC X(24)   VALUE                          
052000                                 'IN-AREA-START  '.                       
052100     SKIP2                                                                
052200 01  IN-AREA.                                                             
052300     03  IN-DATA-AREA          PIC X(300).                                
052400                                                                          
052500*    03  TU1-POST  -COPY W476TU1  -PRE IN-  -RED IN-DATA-AREA             
052600                                                                          
052700*    03  TU2-POST  -COPY W476TU2  -PRE IN-  -RED IN-DATA-AREA             
052800                                                                          
052900     EJECT                                                                
053000 01  FILLER                    PIC X(24)   VALUE                          
053100                                 'SPAR-TU1-AREA   '.                      
053200 01  SPAR-TU1-AREA.                                                       
053300*    03  -COPY W476TU1     -PRE SPAR-.                                    
053400     EJECT                                                                
053500 01  FILLER                    PIC X(24)   VALUE                          
053600                                 'SPAR-TU2-AREA   '.                      
053700 01  SPAR-TU2-AREA.                                                       
053800*    03  -COPY W476TU2     -PRE SPAR-.                                    
053900     EJECT                                                                
054000 01  FILLER                    PIC X(24)   VALUE                          
054100                                 'UT-AREA-START  '.                       
054200                                                                          
054300 01  FILLER                    PIC X(24) VALUE 'S01-AREA'.                
054400 01  S01-AREA.                                                            
054500*    03  -COPY WEDIS01.                                                   
054600                                                                          
054700 01  FILLER                    PIC X(24) VALUE 'G01-AREA'.                
054800 01  G01-AREA.                                                            
054900*    03  -COPY WEDIG01.                                                   
055000                                                                          
055100 01  FILLER                    PIC X(24) VALUE 'G02-AREA'.                
055200 01  G02-AREA.                                                            
055300*    03  -COPY WEDIG02.                                                   
055400                                                                          
055500 01  FILLER                    PIC X(24) VALUE 'H01-AREA'.                
055600 01  H01-AREA.                                                            
055700*    03  -COPY WEDIH01.                                                   
055800                                                                          
055900 01  FILLER                    PIC X(24) VALUE 'H02-AREA'.                
056000 01  H02-AREA.                                                            
056100*    03  -COPY WEDIH02.                                                   
056200                                                                          
056300 01  FILLER                    PIC X(24) VALUE 'H03-AREA'.                
056400 01  H03-AREA.                                                            
056500*    03  -COPY WEDIH03.                                                   
056600                                                                          
056700 01  FILLER                    PIC X(24) VALUE 'H04-AREA'.                
056800 01  H04-AREA.                                                            
056900*    03  -COPY WEDIH04.                                                   
057000                                                                          
057100 01  FILLER                    PIC X(24) VALUE 'H11-AREA'.                
057200 01  H11-AREA.                                                            
057300*    03  -COPY WEDIH11.                                                   
057400                                                                          
057500 01  FILLER                    PIC X(24) VALUE 'H13-AREA'.                
057600 01  H13-AREA.                                                            
057700*    03  -COPY WEDIH13.                                                   
057800                                                                          
057900 01  FILLER                    PIC X(24) VALUE 'H14-AREA'.                
058000 01  H14-AREA.                                                            
058100*    03  -COPY WEDIH14.                                                   
058200                                                                          
058300 01  FILLER                    PIC X(24) VALUE 'H17-AREA'.                
058400 01  H17-AREA.                                                            
058500*    03  -COPY WEDIH17.                                                   
058600                                                                          
058700 01  FILLER                    PIC X(24) VALUE 'H18-AREA'.                
058800 01  H18-AREA.                                                            
058900*    03  -COPY WEDIH18.                                                   
059000                                                                          
059100 01  FILLER                    PIC X(24) VALUE 'H24-AREA'.                
059200 01  H24-AREA.                                                            
059300*    03  -COPY WEDIH24.                                                   
059400                                                                          
059500 01  FILLER                    PIC X(24) VALUE 'H26-AREA'.                
059600 01  H26-AREA.                                                            
059700*    03  -COPY WEDIH26.                                                   
059800                                                                          
059900 01  FILLER                    PIC X(24) VALUE 'H27-AREA'.                
060000 01  H27-AREA.                                                            
060100*    03  -COPY WEDIH27.                                                   
060200                                                                          
060300 01  FILLER                    PIC X(24) VALUE 'A01-AREA'.                
060400 01  A01-AREA.                                                            
060500*    03  -COPY WEDIA01.                                                   
060600                                                                          
060700 01  FILLER                    PIC X(24) VALUE 'A02-AREA'.                
060800 01  A02-AREA.                                                            
060900*    03  -COPY WEDIA02.                                                   
061000                                                                          
061100 01  FILLER                    PIC X(24) VALUE 'A03-AREA'.                
061200 01  A03-AREA.                                                            
061300*    03  -COPY WEDIA03.                                                   
061400                                                                          
061500 01  FILLER                    PIC X(24) VALUE 'A04-MOA-AREA'.            
061600 01  A04-AREA.                                                            
061700*    03  -COPY WEDIA04.                                                   
061800                                                                          
061900 01  FILLER                    PIC X(24) VALUE 'A06-AREA'.                
062000 01  A06-AREA.                                                            
062100*    03  -COPY WEDIA06.                                                   
062200                                                                          
062300 01  FILLER                    PIC X(24) VALUE 'P01-AREA'.                
062400 01  P01-AREA.                                                            
062500*    03  -COPY WEDIP01.                                                   
062600                                                                          
062700 01  FILLER                    PIC X(24) VALUE 'P05-AREA'.                
062800 01  P05-AREA.                                                            
062900*    03  -COPY WEDIP05.                                                   
063000                                                                          
063100 01  FILLER                    PIC X(24) VALUE 'P08-AREA'.                
063200 01  P08-AREA.                                                            
063300*    03  -COPY WEDIP08.                                                   
063400                                                                          
063500 01  FILLER                    PIC X(24) VALUE 'E01-AREA'.                
063600 01  E01-AREA.                                                            
063700*    03  -COPY WEDIE01.                                                   
063800     EJECT                                                                
063900 PROCEDURE DIVISION.                                                      
064000                                                                          
064100 MAIN SECTION.                                                            
064200                                                                          
064300     PERFORM A-INIT                                                       
064400                                                                          
064500     PERFORM S01-LAES-W47680                                              
064600                                                                          
064700     PERFORM UNTIL W47680-EOF                                             
064800                                                                          
064900       IF IN-TU1-IDPTYP = 'TU1'                                           
065000                                                                          
065100           PERFORM B-SKRIV-S01-POST                                       
065200           PERFORM C-SKRIV-SAM-POSTER                                     
065300                                                                          
065400           MOVE IN-AREA          TO SPAR-TU1-AREA                         
065500           MOVE IN-TU1-IDPTYP    TO IN-IDPTYP                             
065600                                                                          
065700           PERFORM D-SKAPA-HUV-POST                                       
065800       ELSE                                                               
065900         IF IN-TU2-IDPTYP = 'TU2'                                         
066000                                                                          
066100           MOVE IN-AREA            TO SPAR-TU2-AREA                       
066200           MOVE IN-TU2-IDPTYP      TO IN-IDPTYP                           
066300                                                                          
066400           PERFORM E-SKAPA-ART-POST                                       
066500                                                                          
066600         END-IF                                                           
066700       END-IF                                                             
066800                                                                          
066900       PERFORM S01-LAES-W47680                                            
067000                                                                          
067100       IF IN-TU1-IDPTYP = 'TU1' OR                                        
067200          W47680-EOF                                                      
067300         PERFORM F-SKAPA-FORP-POSTER                                      
067400         PERFORM G-SKRIV-E01-POST                                         
067500       END-IF                                                             
067600                                                                          
067700     END-PERFORM                                                          
067800                                                                          
067900     PERFORM Z-FINIT                                                      
068000                                                                          
068100     MOVE ZERO TO RETURN-CODE                                             
068200     GOBACK                                                               
068300     .                                                                    
068400     EJECT                                                                
068500 A-INIT SECTION.                                                          
068600     MOVE 'A-INIT             ' TO WS-SEKTION                             
068700                                                                          
068800     OPEN INPUT  W47680                                                   
068900                                                                          
069000     OPEN OUTPUT W47681                                                   
069100                 W47681X                                                  
069200                                                                          
069300     ACCEPT DAGENS-DATUM  FROM DATE                                       
069400     ACCEPT DAGENS-TIME   FROM TIME                                       
069500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
069600                                                                          
069700     MOVE ZERO  TO WS-KOLLI-RAK                                           
069800     .                                                                    
069900     EJECT                                                                
070000 B-SKRIV-S01-POST SECTION.                                                
070100     MOVE 'B-SKRIV-S01        ' TO WS-SEKTION                             
070200                                                                          
070300     INITIALIZE S01-AREA                                                  
070400                                                                          
070500     MOVE WS-STRECK              TO S01-SLINE                             
070600     MOVE WC-S01-IDPTYP          TO S01-IDPTYP                            
070700                                                                          
070800     IF IN-TU1-KVFAKTUR > ZERO                                            
070900       MOVE WC-S01-G             TO S01-G-TYPE                            
071000       MOVE WC-S01-G02           TO S01-G-POSTER                          
071100     ELSE                                                                 
071200       MOVE WC-S01-G             TO S01-G-TYPE                            
071300       MOVE WC-S01-G00           TO S01-G-POSTER                          
071400     END-IF                                                               
071500                                                                          
071600     MOVE WC-S01-H               TO S01-H-TYPE                            
071700     MOVE WC-S01-H27             TO S01-H-POSTER                          
071800                                                                          
071900     MOVE WC-S01-A               TO S01-A-TYPE                            
072000     MOVE WC-S01-A06             TO S01-A-POSTER                          
072100                                                                          
072200     MOVE WC-S01-P               TO S01-P-TYPE                            
072300     MOVE WC-S01-P08             TO S01-P-POSTER                          
072400                                                                          
072500     MOVE WC-S01-D               TO S01-D-TYPE                            
072600     MOVE WC-S01-D02             TO S01-D-POSTER                          
072700                                                                          
072800     MOVE WC-S01-N               TO S01-N-TYPE                            
072900     MOVE WC-S01-N00             TO S01-N-POSTER                          
073000                                                                          
073100     MOVE WC-S01-E               TO S01-E-TYPE                            
073200     MOVE WC-S01-E01             TO S01-E-POSTER                          
073300                                                                          
073400     WRITE UT-S01-POST           FROM S01-AREA                            
073500     MOVE S01-AREA(1:3)          TO EDI-IDPTYP                            
073600     PERFORM S21-POSTSUM-UTPOST                                           
073700                                                                          
073800     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
073900             WS-NUMBER-OF-SEGMENTS + 1                                    
074000                                                                          
074100     MOVE IN-TU1-IDDISTR         TO TEST-IDDISTR                          
074200     IF DIST42-EJ-EU-MIC OR                                               
074300        DIST42-OVR-MIC                                                    
074400                                                                          
074500       WRITE UT2-S01-POST        FROM S01-AREA                            
074600       MOVE S01-AREA(1:3)        TO EDI-IDPTYP                            
074700       PERFORM S22-POSTSUM-UTPOST                                         
074800                                                                          
074900     END-IF                                                               
075000     .                                                                    
075100     EJECT                                                                
075200 C-SKRIV-SAM-POSTER SECTION.                                              
075300     MOVE 'C-SKRIV-SAM        ' TO WS-SEKTION                             
075400                                                                          
075500     IF IN-TU1-KVFAKTUR > ZERO                                            
075600                                                                          
075700       INITIALIZE G01-AREA                                                
075800                  G02-AREA                                                
075900                                                                          
076000       PERFORM CA-SKAPA-G01-POST                                          
076100       PERFORM CB-SKAPA-G02-POST                                          
076200     END-IF                                                               
076300     .                                                                    
076400     EJECT                                                                
076500 CA-SKAPA-G01-POST SECTION.                                               
076600     MOVE 'CA-SKAPA-G01       ' TO WS-SEKTION                             
076700                                                                          
076800     MOVE WS-STRECK                TO G01-SLINE                           
076900     MOVE WC-G01-IDPTYP            TO G01-IDPTYP                          
077000                                                                          
077100     MOVE IN-TU1-IDTULL            TO G01-NUMBER                          
077200     MOVE IN-TU1-IDUSER            TO G01-USER-ID                         
077300     MOVE WC-G01-COMPANY-CODE      TO G01-COMPANY-CODE                    
077400     MOVE IN-TU1-IDKUNDNR          TO G01-SHORT-NAME                      
077500     MOVE IN-TU1-BEKOPARE-RAD1     TO G01-CONSIGNEE-1                     
077600     IF IN-TU1-FLSLUT = JA OR YES                                         
077700       MOVE WC-G01-ETT             TO G01-LAST-INVOICE                    
077800     ELSE                                                                 
077900       MOVE WC-G01-ZERO            TO G01-LAST-INVOICE                    
078000     END-IF                                                               
078100                                                                          
078200     WRITE UT-G01-POST               FROM G01-AREA                        
078300     MOVE G01-AREA(1:3)              TO EDI-IDPTYP                        
078400     PERFORM S21-POSTSUM-UTPOST                                           
078500                                                                          
078600     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
078700             WS-NUMBER-OF-SEGMENTS + 1                                    
078800     .                                                                    
078900     EJECT                                                                
079000 CB-SKAPA-G02-POST SECTION.                                               
079100     MOVE 'CB-SKAPA-G02       '    TO WS-SEKTION                          
079200                                                                          
079300     MOVE WS-STRECK                TO G02-SLINE                           
079400     MOVE WC-G02-IDPTYP            TO G02-IDPTYP                          
079500                                                                          
079600     MOVE WC-G02-INPUT-TYPE        TO G02-INPUT-TYPE                      
079700                                                                          
079800     PERFORM S11-TA-FRAM-TRPSATT                                          
079900     MOVE WS-KDTRPSATT             TO G02-CARRIAGE-CODE                   
080000                                                                          
080100     WRITE UT-G02-POST               FROM G02-AREA                        
080200     MOVE G02-AREA(1:3)              TO EDI-IDPTYP                        
080300     PERFORM S21-POSTSUM-UTPOST                                           
080400                                                                          
080500     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
080600             WS-NUMBER-OF-SEGMENTS + 1                                    
080700     .                                                                    
080800     EJECT                                                                
080900 D-SKAPA-HUV-POST SECTION.                                                
081000     MOVE 'D-SKAPA-HUV        '    TO WS-SEKTION                          
081100                                                                          
081200     INITIALIZE H01-AREA                                                  
081300                H02-AREA                                                  
081400                H03-AREA                                                  
081500                H04-AREA                                                  
081600                H11-AREA                                                  
081700                H13-AREA                                                  
081800                H14-AREA                                                  
081900                H17-AREA                                                  
082000                H18-AREA                                                  
082100                H24-AREA                                                  
082200                H26-AREA                                                  
082300                H27-AREA                                                  
082400     MOVE ZERO TO WS-A01-LINE                                             
082500                                                                          
082600     PERFORM DA-SKRIV-H01-POST                                            
082700     PERFORM DB-SKRIV-H02-POST                                            
082800     PERFORM DC-SKRIV-H03-POST                                            
082900     PERFORM DD-SKRIV-H04-POST                                            
083000     PERFORM DE-SKRIV-H11-POST                                            
083100     PERFORM DF-SKRIV-H13-POST                                            
083200     PERFORM DG-SKRIV-H14-POST                                            
083300     PERFORM DH-SKRIV-H17-POST                                            
083400     PERFORM DI-SKRIV-H18-POST                                            
083500     PERFORM DJ-SKRIV-H24-POST                                            
083600     PERFORM DK-SKRIV-H26-POST                                            
083700     PERFORM DL-SKRIV-H27-POST                                            
083800     .                                                                    
083900     EJECT                                                                
084000 DA-SKRIV-H01-POST SECTION.                                               
084100     MOVE 'DA-SKRIV-H01       '    TO WS-SEKTION                          
084200                                                                          
084300     MOVE WS-STRECK                TO H01-SLINE                           
084400     MOVE WC-H01-IDPTYP            TO H01-IDPTYP                          
084500                                                                          
084600     MOVE WC-H01-INPUT-TYPE        TO H01-INPUT-TYPE                      
084700     MOVE IN-TU1-IDFAKT            TO H01-IDFAKT                          
084800     MOVE IN-TU1-IDKUNDNR          TO H01-SHORT-NAME                      
084900                                                                          
085000     PERFORM S11-TA-FRAM-TRPSATT                                          
085100     MOVE WS-KDTRPSATT             TO H01-CARRIAGE-CODE                   
085200                                                                          
085300     MOVE IN-TU1-TIFAKT            TO WS-TIFAKT                           
085400     MOVE WS-T-AR                  TO WS-TIFAKT-A2                        
085500     MOVE WS-T-RESTEN              TO WS-TIFAKT-RESTEN                    
085600     MOVE WS-TVA                   TO WS-TIFAKT-A1                        
085700     MOVE WS-TIFAKT-HEL            TO H01-INVOICE-DATE-DELIVERY           
085800                                                                          
085900     WRITE UT-H01-POST               FROM H01-AREA                        
086000     MOVE H01-AREA(1:3)              TO EDI-IDPTYP                        
086100     PERFORM S21-POSTSUM-UTPOST                                           
086200                                                                          
086300     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
086400             WS-NUMBER-OF-SEGMENTS + 1                                    
086500                                                                          
086600     MOVE IN-TU1-IDDISTR           TO TEST-IDDISTR                        
086700     IF DIST42-EJ-EU-MIC OR                                               
086800        DIST42-OVR-MIC                                                    
086900                                                                          
087000       MOVE IN-TU1-IDFAKT          TO H01-IDFAKT                          
087100       MOVE WS-LINJE               TO WS-DATUM-0                          
087200       MOVE WS-TJUGO               TO WS-DATUM-AA1                        
087300       MOVE DAGENS-DATUM-AAR       TO WS-DATUM-AA2                        
087400       MOVE WS-DATUM-HEL           TO H01-DATE-YYYY                       
087500       MOVE WS-DATUM-AA1           TO WS-DAT-AA1                          
087600       MOVE WS-DATUM-AA2           TO WS-DAT-AA2                          
087700       MOVE DAGENS-DATUM-MM        TO WS-DAT-MM                           
087800       MOVE DAGENS-DATUM-DD        TO WS-DAT-DD                           
087900       MOVE WS-CURRENT-HHMM        TO WS-DAT-HHMM                         
088000       MOVE WS-DATUM-TID           TO H01-INVOICE-DATE-DELIVERY           
088100                                                                          
088200       MOVE WS-TIFAKT-HEL          TO H18-INVOICE-DATE-YYYYMMDD           
088300       MOVE IN-TU1-DARFS           TO WS-DARFS-CHA                        
088400       MOVE WS-DARFS               TO H18-RFS-DATE-YYYYMMDDHHMM           
088500                                                                          
088600       WRITE UT2-H01-POST        FROM H01-AREA                            
088700       MOVE H01-AREA(1:3)        TO EDI-IDPTYP                            
088800       PERFORM S22-POSTSUM-UTPOST                                         
088900                                                                          
089000     END-IF                                                               
089100     .                                                                    
089200     EJECT                                                                
089300 DB-SKRIV-H02-POST SECTION.                                               
089400     MOVE 'DB-SKRIV-H02       '    TO WS-SEKTION                          
089500                                                                          
089600     MOVE WS-STRECK                TO H02-SLINE                           
089700     MOVE WC-H02-IDPTYP            TO H02-IDPTYP                          
089800                                                                          
089900     MOVE IN-TU1-IDKUNDNR          TO H02-CONSIGNEE-CODE                  
090000                                                                          
090100     WRITE UT-H02-POST               FROM H02-AREA                        
090200     MOVE H02-AREA(1:3)              TO EDI-IDPTYP                        
090300     PERFORM S21-POSTSUM-UTPOST                                           
090400                                                                          
090500     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
090600             WS-NUMBER-OF-SEGMENTS + 1                                    
090700                                                                          
090800     IF DIST42-EJ-EU-MIC OR                                               
090900        DIST42-OVR-MIC                                                    
091000                                                                          
091100       WRITE UT2-H02-POST        FROM H02-AREA                            
091200       MOVE H02-AREA(1:3)        TO EDI-IDPTYP                            
091300       PERFORM S22-POSTSUM-UTPOST                                         
091400                                                                          
091500     END-IF                                                               
091600     .                                                                    
091700     EJECT                                                                
091800 DC-SKRIV-H03-POST SECTION.                                               
091900     MOVE 'DC-SKRIV-H03       '    TO WS-SEKTION                          
092000                                                                          
092100     MOVE WS-STRECK                TO H03-SLINE                           
092200     MOVE WC-H03-IDPTYP            TO H03-IDPTYP                          
092300                                                                          
092400     MOVE IN-TU1-BEKOPARE-RAD1     TO H03-CONSIGNEE-1                     
092500     MOVE IN-TU1-BEKOPARE-RAD2     TO H03-CONSIGNEE-2                     
092600     MOVE IN-TU1-ADKOPARE-RAD1     TO H03-CONSIGNEE-3                     
092700                                                                          
092800     WRITE UT-H03-POST               FROM H03-AREA                        
092900     MOVE H03-AREA(1:3)              TO EDI-IDPTYP                        
093000     PERFORM S21-POSTSUM-UTPOST                                           
093100                                                                          
093200     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
093300             WS-NUMBER-OF-SEGMENTS + 1                                    
093400                                                                          
093500     IF DIST42-EJ-EU-MIC OR                                               
093600        DIST42-OVR-MIC                                                    
093700                                                                          
093800       WRITE UT2-H03-POST        FROM H03-AREA                            
093900       MOVE H03-AREA(1:3)        TO EDI-IDPTYP                            
094000       PERFORM S22-POSTSUM-UTPOST                                         
094100                                                                          
094200     END-IF                                                               
094300     .                                                                    
094400     EJECT                                                                
094500 DD-SKRIV-H04-POST SECTION.                                               
094600     MOVE 'DD-SKRIV-H04       '    TO WS-SEKTION                          
094700                                                                          
094800     MOVE WS-STRECK                TO H04-SLINE                           
094900     MOVE WC-H04-IDPTYP            TO H04-IDPTYP                          
095000                                                                          
095100     MOVE IN-TU1-ADKOPARE-RAD2     TO H04-CONSIGNEE-4                     
095200                                                                          
095300     WRITE UT-H04-POST               FROM H04-AREA                        
095400     MOVE H04-AREA(1:3)              TO EDI-IDPTYP                        
095500     PERFORM S21-POSTSUM-UTPOST                                           
095600                                                                          
095700     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
095800             WS-NUMBER-OF-SEGMENTS + 1                                    
095900                                                                          
096000     IF DIST42-EJ-EU-MIC OR                                               
096100        DIST42-OVR-MIC                                                    
096200                                                                          
096300       WRITE UT2-H04-POST        FROM H04-AREA                            
096400       MOVE H04-AREA(1:3)        TO EDI-IDPTYP                            
096500       PERFORM S22-POSTSUM-UTPOST                                         
096600                                                                          
096700     END-IF                                                               
096800     .                                                                    
096900     EJECT                                                                
097000 DE-SKRIV-H11-POST SECTION.                                               
097100     MOVE 'DE-SKRIV-H11       '    TO WS-SEKTION                          
097200                                                                          
097300     MOVE WS-STRECK                TO H11-SLINE                           
097400     MOVE WC-H11-IDPTYP            TO H11-IDPTYP                          
097500                                                                          
097600     IF IN-TU1-KDTRPTYP = 1                                               
097700       MOVE 'SHIP'                 TO H11-MODE-OF-TRANSPORT               
097800     END-IF                                                               
097900                                                                          
098000     IF IN-TU1-KDTRPTYP = 2                                               
098100       MOVE 'TRAIN'                TO H11-MODE-OF-TRANSPORT               
098200     END-IF                                                               
098300                                                                          
098400     IF IN-TU1-KDTRPTYP = 3                                               
098500       MOVE 'TRUCK'                TO H11-MODE-OF-TRANSPORT               
098600     END-IF                                                               
098700                                                                          
098800     IF IN-TU1-KDTRPTYP = 4                                               
098900       MOVE 'FLYG'                 TO H11-MODE-OF-TRANSPORT               
099000     END-IF                                                               
099100                                                                          
099200     MOVE IN-TU1-IDDISTR           TO BEST-IDDISTR                        
099300     CALL W475BEST USING BEST-W475BEST                                    
099400     MOVE BEST-IDLANDX3            TO WS-BESTLAND                         
099500                                                                          
099600     IF IN-TU1-IDDISTR = 6480                                             
099700       IF IN-TU1-IDKUNDNR = 0                                             
099800         MOVE 'CL'                 TO WS-BESTLAND                         
099900       END-IF                                                             
100000                                                                          
100100       IF IN-TU1-IDKUNDNR = 100                                           
100200         MOVE 'BO'                 TO WS-BESTLAND                         
100300       END-IF                                                             
100400                                                                          
100500       IF IN-TU1-IDKUNDNR = 200                                           
100600         MOVE 'AR'                 TO WS-BESTLAND                         
100700       END-IF                                                             
100800                                                                          
100900     END-IF                                                               
101000                                                                          
101100     IF IN-TU1-IDDISTR = 7490                                             
101200       IF IN-TU1-IDKUNDNR = 100                                           
101300         MOVE 'GT'                 TO WS-BESTLAND                         
101400       END-IF                                                             
101500                                                                          
101600       IF IN-TU1-IDKUNDNR = 200                                           
101700         MOVE 'SV'                 TO WS-BESTLAND                         
101800       END-IF                                                             
101900                                                                          
102000       IF IN-TU1-IDKUNDNR = 300                                           
102100         MOVE 'HN'                 TO WS-BESTLAND                         
102200       END-IF                                                             
102300                                                                          
102400       IF IN-TU1-IDKUNDNR = 500                                           
102500         MOVE 'NI'                 TO WS-BESTLAND                         
102600       END-IF                                                             
102700                                                                          
102800       IF IN-TU1-IDKUNDNR = 701                                           
102900         MOVE 'TT'                 TO WS-BESTLAND                         
103000       END-IF                                                             
103100                                                                          
103200     END-IF                                                               
103300                                                                          
103400     MOVE WS-BESTLAND              TO H11-DESTINATION                     
103500                                                                          
103600     WRITE UT-H11-POST             FROM H11-AREA                          
103700     MOVE H11-AREA(1:3)            TO EDI-IDPTYP                          
103800     PERFORM S21-POSTSUM-UTPOST                                           
103900                                                                          
104000     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
104100             WS-NUMBER-OF-SEGMENTS + 1                                    
104200                                                                          
104300     IF DIST42-EJ-EU-MIC OR                                               
104400        DIST42-OVR-MIC                                                    
104500                                                                          
104600       MOVE IN-TU1-KDTRPTYP        TO H11-MODE-OF-TRANSPORT               
104700                                                                          
104800       IF IN-TU1-FLCONTAIN = JA OR YES                                    
104900         MOVE '1'                  TO H11-FULL-CONTAINER                  
105000       ELSE                                                               
105100         MOVE '0'                  TO H11-FULL-CONTAINER                  
105200       END-IF                                                             
105300                                                                          
105400       MOVE IN-TU1-IDFORDREG       TO H11-VEHICLE-NO                      
105500                                                                          
105600       WRITE UT2-H11-POST        FROM H11-AREA                            
105700       MOVE  H11-AREA(1:3)       TO EDI-IDPTYP                            
105800       PERFORM S22-POSTSUM-UTPOST                                         
105900                                                                          
106000     END-IF                                                               
106100     .                                                                    
106200     EJECT                                                                
106300 DF-SKRIV-H13-POST SECTION.                                               
106400     MOVE 'DF-SKRIV-H13       '    TO WS-SEKTION                          
106500                                                                          
106600     MOVE WS-STRECK                TO H13-SLINE                           
106700     MOVE WC-H13-IDPTYP            TO H13-IDPTYP                          
106800                                                                          
106900     MOVE IN-TU1-BEKOPARE-RAD1     TO H13-MARKS-1                         
107000                                                                          
107010     MOVE IN-TU1-IDSIGILL          TO H13-IDSIGILL                        
107020                                                                          
107100     WRITE UT-H13-POST             FROM H13-AREA                          
107200     MOVE H13-AREA(1:3)            TO EDI-IDPTYP                          
107300     PERFORM S21-POSTSUM-UTPOST                                           
107400                                                                          
107500     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
107600             WS-NUMBER-OF-SEGMENTS + 1                                    
107700                                                                          
107800     IF DIST42-EJ-EU-MIC OR                                               
107900        DIST42-OVR-MIC                                                    
108000                                                                          
108100       MOVE IN-TU1-IDPARTNR        TO H13-IDPARTNR                        
108200                                                                          
108300       WRITE UT2-H13-POST        FROM H13-AREA                            
108400       MOVE  H13-AREA(1:3)       TO EDI-IDPTYP                            
108500       PERFORM S22-POSTSUM-UTPOST                                         
108600                                                                          
108700     END-IF                                                               
108800     .                                                                    
108900     EJECT                                                                
109000 DG-SKRIV-H14-POST SECTION.                                               
109100     MOVE 'DG-SKRIV-H14       '    TO WS-SEKTION                          
109200                                                                          
109300     MOVE WS-STRECK                TO H14-SLINE                           
109400     MOVE WC-H14-IDPTYP            TO H14-IDPTYP                          
109500                                                                          
109600     MOVE IN-TU1-BEKOPARE-RAD2     TO H14-MARKS-2                         
109700     MOVE IN-TU1-ADKOPARE-RAD1     TO H14-MARKS-3                         
109800     MOVE IN-TU1-ADKOPARE-RAD2     TO H14-MARKS-4                         
109900                                                                          
110000     WRITE UT-H14-POST             FROM H14-AREA                          
110100     MOVE H14-AREA(1:3)            TO EDI-IDPTYP                          
110200     PERFORM S21-POSTSUM-UTPOST                                           
110300                                                                          
110400     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
110500             WS-NUMBER-OF-SEGMENTS + 1                                    
110600                                                                          
110700     IF DIST42-EJ-EU-MIC OR                                               
110800        DIST42-OVR-MIC                                                    
110900                                                                          
111000       WRITE UT2-H14-POST        FROM H14-AREA                            
111100       MOVE  H14-AREA(1:3)       TO EDI-IDPTYP                            
111200       PERFORM S22-POSTSUM-UTPOST                                         
111300                                                                          
111400     END-IF                                                               
111500     .                                                                    
111600     EJECT                                                                
111700 DH-SKRIV-H17-POST SECTION.                                               
111800     MOVE 'DH-SKRIV-H17       '    TO WS-SEKTION                          
111900                                                                          
112000     MOVE WS-STRECK                TO H17-SLINE                           
112100     MOVE WC-H17-IDPTYP            TO H17-IDPTYP                          
112200                                                                          
112300     MOVE IN-TU1-KDVALISO          TO H17-CURRENCY                        
112400     MOVE WS-BESTLAND              TO H17-DESTINATION-ISO-CO              
112500     MOVE IN-TU1-IDUSER            TO H17-USER-ID                         
112600     MOVE WC-G01-COMPANY-CODE      TO H17-COMPANY-CODE                    
112700                                                                          
112800     MOVE IN-TU1-IDDC              TO WS-IDDC                             
112900                                                                          
113000     IF IN-TU1-IDDC                =  WC-CDC-SE OR                        
113100                                      WC-DDC-SE                           
113200       MOVE WC-H17-ORIGIN-COUNTRY  TO H17-ORIGIN-COUNTRY                  
113300     END-IF                                                               
113400                                                                          
113500     IF IN-TU1-IDDC                =  WC-SDC-ES                           
113600       MOVE WC-H17-ORIGIN-COUNTRY-2                                       
113700                                   TO H17-ORIGIN-COUNTRY                  
113800     END-IF                                                               
113900                                                                          
114000     IF IN-TU1-IDDC                =  WC-NDC-AE                           
114100       MOVE WC-H17-ORIGIN-COUNTRY-3                                       
114200                                   TO H17-ORIGIN-COUNTRY                  
114300     END-IF                                                               
114400                                                                          
114500     IF IN-TU1-IDDC                =  WC-SDC-NL                           
114600       MOVE WC-H17-ORIGIN-COUNTRY-4                                       
114700                                   TO H17-ORIGIN-COUNTRY                  
114800     END-IF                                                               
114900                                                                          
115000     IF NDC-US                                                            
115100       MOVE WC-H17-ORIGIN-COUNTRY-5                                       
115200                                   TO H17-ORIGIN-COUNTRY                  
115300     END-IF                                                               
115400                                                                          
115500     IF NDC-CN                                                            
115600       MOVE WC-H17-ORIGIN-COUNTRY-6                                       
115700                                   TO H17-ORIGIN-COUNTRY                  
115800     END-IF                                                               
115900                                                                          
116000     MOVE IN-TU1-SUORDV-FAKT       TO WS-SUARTNTO-NUM                     
116100     MOVE WS-SUARTNTO-HEL          TO WS-S-HEL                            
116200     MOVE WS-SUARTNTO-DEC          TO WS-S-DEC                            
116300     MOVE WS-PUNKT                 TO WS-S-PUNKT                          
116400                                                                          
116500     MOVE WS-SUARTNTO-PUNKT        TO H17-INVOICE-TOTAL                   
116600                                                                          
116700     WRITE UT-H17-POST             FROM H17-AREA                          
116800     MOVE H17-AREA(1:3)            TO EDI-IDPTYP                          
116900     PERFORM S21-POSTSUM-UTPOST                                           
117000                                                                          
117100     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
117200             WS-NUMBER-OF-SEGMENTS + 1                                    
117300                                                                          
117400     IF DIST42-EJ-EU-MIC OR                                               
117500        DIST42-OVR-MIC                                                    
117600                                                                          
117700       WRITE UT2-H17-POST        FROM H17-AREA                            
117800       MOVE  H17-AREA(1:3)       TO EDI-IDPTYP                            
117900       PERFORM S22-POSTSUM-UTPOST                                         
118000                                                                          
118100     END-IF                                                               
118200     .                                                                    
118300     EJECT                                                                
118400 DI-SKRIV-H18-POST SECTION.                                               
118500     MOVE 'DI-SKRIV-H18       '    TO WS-SEKTION                          
118600                                                                          
118700     MOVE WS-STRECK                TO H18-SLINE                           
118800     MOVE WC-H18-IDPTYP            TO H18-IDPTYP                          
118900                                                                          
119000     MOVE IN-TU1-IDFAKT            TO H18-INVOICE-NO                      
119100                                                                          
119200     MOVE IN-TU1-IDDC              TO H18-TRANS-CARR-ISO-AVG              
119300                                                                          
119400     IF IN-TU1-IDDC                =  WC-CDC-SE OR                        
119500                                      WC-DDC-SE                           
119600       MOVE WC-H18-ISO-SE          TO H18-TRANS-CARR-ISO-GRP              
119700     END-IF                                                               
119800                                                                          
119900     IF IN-TU1-IDDC                =  WC-SDC-ES                           
120000       MOVE WC-H18-ISO-ES          TO H18-TRANS-CARR-ISO-GRP              
120100     END-IF                                                               
120200                                                                          
120300     IF IN-TU1-IDDC                =  WC-NDC-AE                           
120400       MOVE WC-H18-ISO-AE          TO H18-TRANS-CARR-ISO-GRP              
120500     END-IF                                                               
120600                                                                          
120700     IF IN-TU1-IDDC                =  WC-SDC-NL                           
120800       MOVE WC-H18-ISO-NL          TO H18-TRANS-CARR-ISO-GRP              
120900     END-IF                                                               
121000                                                                          
121100     IF NDC-US                                                            
121200       MOVE WC-H18-ISO-US          TO H18-TRANS-CARR-ISO-GRP              
121300     END-IF                                                               
121400                                                                          
121500     IF NDC-CN                                                            
121600       MOVE WC-H18-ISO-CN          TO H18-TRANS-CARR-ISO-GRP              
121700     END-IF                                                               
121800                                                                          
121900     WRITE UT-H18-POST             FROM H18-AREA                          
122000     MOVE H18-AREA(1:3)            TO EDI-IDPTYP                          
122100     PERFORM S21-POSTSUM-UTPOST                                           
122200                                                                          
122300     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
122400             WS-NUMBER-OF-SEGMENTS + 1                                    
122500                                                                          
122600     IF DIST42-EJ-EU-MIC OR                                               
122700        DIST42-OVR-MIC                                                    
122800                                                                          
122900       WRITE UT2-H18-POST        FROM H18-AREA                            
123000       MOVE  H18-AREA(1:3)       TO EDI-IDPTYP                            
123100       PERFORM S22-POSTSUM-UTPOST                                         
123200                                                                          
123300     END-IF                                                               
123400     .                                                                    
123500     EJECT                                                                
123600 DJ-SKRIV-H24-POST SECTION.                                               
123700     MOVE 'DJ-SKRIV-H24       '    TO WS-SEKTION                          
123800                                                                          
123900     MOVE WS-STRECK                TO H24-SLINE                           
124000     MOVE WC-H24-IDPTYP            TO H24-IDPTYP                          
124100                                                                          
124200     MOVE IN-TU1-IDTULL            TO H24-CUS-ID                          
124300                                                                          
124400     WRITE UT-H24-POST             FROM H24-AREA                          
124500     MOVE H24-AREA(1:3)            TO EDI-IDPTYP                          
124600     PERFORM S21-POSTSUM-UTPOST                                           
124700                                                                          
124800     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
124900             WS-NUMBER-OF-SEGMENTS + 1                                    
125000                                                                          
125100     IF DIST42-EJ-EU-MIC OR                                               
125200        DIST42-OVR-MIC                                                    
125300                                                                          
125400       MOVE IN-TU1-BELEVVIL        TO H24-BELEVVIL                        
125500                                                                          
125600       IF DIST42-MIC-EGYPT                                                
125700         MOVE 'CIP            (INCOTERMS 2010)'                           
125800                                   TO H24-BELEVVIL                        
125900       END-IF                                                             
126000                                                                          
126100       WRITE UT2-H24-POST        FROM H24-AREA                            
126200       MOVE  H24-AREA(1:3)       TO EDI-IDPTYP                            
126300       PERFORM S22-POSTSUM-UTPOST                                         
126400                                                                          
126500     END-IF                                                               
126600     .                                                                    
126700     EJECT                                                                
126800 DK-SKRIV-H26-POST SECTION.                                               
126900     MOVE 'DK-SKRIV-H26       '    TO WS-SEKTION                          
127000                                                                          
127100     MOVE WS-STRECK                TO H26-SLINE                           
127200     MOVE WC-H26-IDPTYP            TO H26-IDPTYP                          
127300                                                                          
127400     MOVE IN-TU1-IDBOKN            TO H26-BOOKING-REF-1                   
127500                                                                          
127600     WRITE UT-H26-POST             FROM H26-AREA                          
127700     MOVE H26-AREA(1:3)            TO EDI-IDPTYP                          
127800     PERFORM S21-POSTSUM-UTPOST                                           
127900                                                                          
128000     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
128100             WS-NUMBER-OF-SEGMENTS + 1                                    
128200                                                                          
128300     IF DIST42-EJ-EU-MIC OR                                               
128400        DIST42-OVR-MIC                                                    
128500                                                                          
128600       MOVE WS-PUNKT               TO WS-A-PUNKT                          
128700                                                                          
128800       MOVE IN-TU1-PREMBHNT        TO WS-ADDITIONAL-NUM                   
128900       MOVE WS-ADDITIONAL-HEL      TO WS-A-HEL                            
129000       MOVE WS-ADDITIONAL-DEC      TO WS-A-DEC                            
129100                                                                          
129200       MOVE WS-ADDITIONAL-PUNKT    TO H26-PREMBHNT                        
129300                                                                          
129400       MOVE IN-TU1-PRFRAKT         TO WS-ADDITIONAL-NUM                   
129500       MOVE WS-ADDITIONAL-HEL      TO WS-A-HEL                            
129600       MOVE WS-ADDITIONAL-DEC      TO WS-A-DEC                            
129700                                                                          
129800       MOVE WS-ADDITIONAL-PUNKT    TO H26-PRFRAKT                         
129900                                                                          
130000       MOVE IN-TU1-PRFOERS         TO WS-ADDITIONAL-NUM                   
130100       MOVE WS-ADDITIONAL-HEL      TO WS-A-HEL                            
130200       MOVE WS-ADDITIONAL-DEC      TO WS-A-DEC                            
130300                                                                          
130400       MOVE WS-ADDITIONAL-PUNKT    TO H26-PRFOERS                         
130500                                                                          
130600       MOVE IN-TU1-PRLEGKST        TO WS-ADDITIONAL-NUM                   
130700       MOVE WS-ADDITIONAL-HEL      TO WS-A-HEL                            
130800       MOVE WS-ADDITIONAL-DEC      TO WS-A-DEC                            
130900                                                                          
131000       MOVE WS-ADDITIONAL-PUNKT    TO H26-PRLEGKST                        
131100                                                                          
131200       WRITE UT2-H26-POST        FROM H26-AREA                            
131300       MOVE  H26-AREA(1:3)       TO EDI-IDPTYP                            
131400       PERFORM S22-POSTSUM-UTPOST                                         
131500                                                                          
131600     END-IF                                                               
131700     .                                                                    
131800     EJECT                                                                
131900 DL-SKRIV-H27-POST SECTION.                                               
132000     MOVE 'DL-SKRIV-H27       '    TO WS-SEKTION                          
132100                                                                          
132200     MOVE WS-STRECK                TO H27-SLINE                           
132300     MOVE WC-H27-IDPTYP            TO H27-IDPTYP                          
132400                                                                          
132500     MOVE IN-TU1-IDDISTR           TO TEST-IDDISTR                        
132600     IF IN-TU1-KDGRANS = 609 AND NOT DIST03-NORGE                         
132700       MOVE 'GGL'                  TO H27-PLACE-OF-LOADING                
132800     ELSE                                                                 
132900       MOVE 'IXN'                  TO H27-PLACE-OF-LOADING                
133000     END-IF                                                               
133100                                                                          
133200     WRITE UT-H27-POST               FROM H27-AREA                        
133300     MOVE H27-AREA(1:3)              TO EDI-IDPTYP                        
133400     PERFORM S21-POSTSUM-UTPOST                                           
133500                                                                          
133600     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
133700             WS-NUMBER-OF-SEGMENTS + 1                                    
133800                                                                          
133900     IF DIST42-EJ-EU-MIC OR                                               
134000        DIST42-OVR-MIC                                                    
134100                                                                          
134200       WRITE UT2-H27-POST        FROM H27-AREA                            
134300       MOVE  H27-AREA(1:3)       TO EDI-IDPTYP                            
134400       PERFORM S22-POSTSUM-UTPOST                                         
134500                                                                          
134600     END-IF                                                               
134700     .                                                                    
134800     EJECT                                                                
134900 E-SKAPA-ART-POST SECTION.                                                
135000     MOVE 'E-SKAPA-ART        '    TO WS-SEKTION                          
135100                                                                          
135200     INITIALIZE A01-AREA                                                  
135300                A02-AREA                                                  
135400                A03-AREA                                                  
135500                A04-AREA                                                  
135600                A06-AREA                                                  
135700                                                                          
135800     PERFORM EA-SKRIV-A01-POST                                            
135900     PERFORM EB-SKRIV-A02-POST                                            
136000     PERFORM EC-SKRIV-A03-POST                                            
136100     PERFORM ED-SKRIV-A04-POST                                            
136200     PERFORM EE-SKRIV-A06-POST                                            
136300     .                                                                    
136400     EJECT                                                                
136500 EA-SKRIV-A01-POST SECTION.                                               
136600     MOVE 'EA-SKRIV-A01       '    TO WS-SEKTION                          
136700                                                                          
136800     MOVE WS-STRECK                TO A01-SLINE                           
136900     MOVE WC-A01-IDPTYP            TO A01-IDPTYP                          
137000                                                                          
137100     MOVE SPAR-TU1-IDFAKT          TO A01-IDFAKT                          
137200                                                                          
137300     COMPUTE WS-A01-LINE = WS-A01-LINE + 1                                
137400     MOVE WS-A01-LINE              TO A01-LINE                            
137500                                                                          
137600     MOVE IN-TU2-IDARTNR           TO A01-ARTICLE-NUMBER                  
137700     MOVE WC-A01-BILDELAR          TO A01-KDSORT                          
137800                                                                          
137900     WRITE UT-A01-POST               FROM A01-AREA                        
138000     MOVE A01-AREA(1:3)              TO EDI-IDPTYP                        
138100     PERFORM S21-POSTSUM-UTPOST                                           
138200                                                                          
138300     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
138400             WS-NUMBER-OF-SEGMENTS + 1                                    
138500                                                                          
138600     MOVE SPAR-TU1-IDDISTR           TO TEST-IDDISTR                      
138700     IF DIST42-EJ-EU-MIC OR                                               
138800        DIST42-OVR-MIC                                                    
138900                                                                          
139000       EVALUATE TRUE                                                      
139100       WHEN IN-TU2-KDSORT = 'ST'                                          
139200         MOVE 'PCE'                  TO A01-KDSORT                        
139300       WHEN IN-TU2-KDSORT = 'SA'                                          
139400         MOVE 'PCE'                  TO A01-KDSORT                        
139500       WHEN IN-TU2-KDSORT = 'L '                                          
139600         MOVE 'LTR'                  TO A01-KDSORT                        
139700       WHEN IN-TU2-KDSORT = 'M '                                          
139800         MOVE 'MTR'                  TO A01-KDSORT                        
139900       WHEN IN-TU2-KDSORT = 'HW'                                          
140000         MOVE 'PCE'                  TO A01-KDSORT                        
140100       WHEN OTHER                                                         
140200         MOVE 'PCE'                  TO A01-KDSORT                        
140300       END-EVALUATE                                                       
140400                                                                          
140410       MOVE IN-TU2-BEPSNUN           TO A01-BEPSN-UN                      
140420                                                                          
140500       WRITE UT2-A01-POST            FROM A01-AREA                        
140600       MOVE A01-AREA(1:3)            TO EDI-IDPTYP                        
140700       PERFORM S22-POSTSUM-UTPOST                                         
140800                                                                          
140900     END-IF                                                               
141000     .                                                                    
141100     EJECT                                                                
141200 EB-SKRIV-A02-POST SECTION.                                               
141300     MOVE 'EB-SKRIV-A02       '    TO WS-SEKTION                          
141400                                                                          
141500     MOVE WS-STRECK                TO A02-SLINE                           
141600     MOVE WC-A02-IDPTYP            TO A02-IDPTYP                          
141700                                                                          
141800     MOVE IN-TU2-KVLEVART          TO A02-QUANTITY                        
141900                                                                          
142000     IF IN-TU2-PRAVCOST > ZERO                                            
142100       MOVE IN-TU2-PRAVCOST        TO WS-PRARTNTO-NUM                     
142200       MOVE WS-PRARTNTO-HEL        TO WS-P-HEL                            
142300       MOVE WS-PRARTNTO-DEC        TO WS-P-DEC                            
142400       MOVE WS-PUNKT               TO WS-P-PUNKT                          
142500       MOVE WS-PRARTNTO-PUNKT      TO A02-PRICE                           
142600                                      A02-PRICE-SEK                       
142700     ELSE                                                                 
142800       MOVE IN-TU2-PRARTNTO        TO WS-PRARTNTO-NUM                     
142900       MOVE WS-PRARTNTO-HEL        TO WS-P-HEL                            
143000       MOVE WS-PRARTNTO-DEC        TO WS-P-DEC                            
143100       MOVE WS-PUNKT               TO WS-P-PUNKT                          
143200       MOVE WS-PRARTNTO-PUNKT      TO A02-PRICE                           
143300                                      A02-PRICE-SEK                       
143400     END-IF                                                               
143500                                                                          
143600     MOVE SPAR-TU1-IDDISTR         TO TEST-IDDISTR                        
143700     IF DIST79-DEALER-PRICE                                               
143900       MOVE SPAR-TU1-PRKURS        TO EXCH-PRKURS                         
144000*        +1 KDCALL = LOKAL VALUTA TILL SEK                                
144100       MOVE +1                     TO EXCH-KDCALL                         
144200       MOVE IN-TU2-PRARTNTO        TO EXCH-PRARTNTO-IN                    
144300       MOVE +0                     TO EXCH-SUORDV-IN                      
144400       CALL W411EXCH USING  EXCH-W411EXCH                                 
144500                                                                          
144600       MOVE EXCH-PRARTNTO-UT       TO WS-PRARTNTO-NUM                     
144700       MOVE WS-PRARTNTO-HEL        TO WS-P-HEL                            
144800       MOVE WS-PRARTNTO-DEC        TO WS-P-DEC                            
144900       MOVE WS-PUNKT               TO WS-P-PUNKT                          
145000       MOVE WS-PRARTNTO-PUNKT      TO A02-PRICE-SEK                       
145100     END-IF                                                               
145200                                                                          
145300     MOVE IN-TU2-SUARTNTO          TO WS-SUARTNTO-NUM                     
145400     MOVE WS-SUARTNTO-HEL          TO WS-S-HEL                            
145500     MOVE WS-SUARTNTO-DEC          TO WS-S-DEC                            
145600     MOVE WS-PUNKT                 TO WS-S-PUNKT                          
145700     MOVE WS-SUARTNTO-PUNKT        TO A02-AMOUNT                          
145800                                      A02-AMOUNT-SEK                      
145900     IF DIST79-DEALER-PRICE                                               
146100       MOVE SPAR-TU1-PRKURS        TO EXCH-PRKURS                         
146200*        +1 KDCALL = LOKAL VALUTA TILL SEK                                
146300       MOVE +1                     TO EXCH-KDCALL                         
146400       MOVE IN-TU2-SUARTNTO        TO EXCH-SUORDV-IN                      
146500       MOVE +0                     TO EXCH-PRARTNTO-IN                    
146600       CALL W411EXCH USING  EXCH-W411EXCH                                 
146700                                                                          
146800       MOVE EXCH-SUORDV-UT         TO WS-SUARTNTO-NUM                     
146900       MOVE WS-SUARTNTO-HEL        TO WS-S-HEL                            
147000       MOVE WS-SUARTNTO-DEC        TO WS-S-DEC                            
147100       MOVE WS-PUNKT               TO WS-S-PUNKT                          
147200       MOVE WS-SUARTNTO-PUNKT      TO A02-AMOUNT-SEK                      
147300     END-IF                                                               
147400                                                                          
147500*--  VKRADNTO  --> NETTOVIKT UTAN ART.EMBALLAGE                           
147600     MOVE IN-TU2-VKRAD-NTO-KG      TO WS-VKRADNTO-NUM                     
147700     MOVE WS-VKRADNTO-HEL          TO WS-V-HEL                            
147800     MOVE WS-VKRADNTO-DEC          TO WS-V-DEC                            
147900     MOVE WS-PUNKT                 TO WS-V-PUNKT                          
148000     MOVE WS-VKRADNTO-PUNKT        TO A02-WEIGHT-KG                       
148100                                                                          
148200     MOVE IN-TU2-KDARTURS          TO A02-ORIGIN                          
148300                                                                          
148400     MOVE WC-A02-TYPE-ART          TO A02-TYPE-ART                        
148500                                                                          
148600     WRITE UT-A02-POST             FROM A02-AREA                          
148700     MOVE A02-AREA(1:3)            TO EDI-IDPTYP                          
148800     PERFORM S21-POSTSUM-UTPOST                                           
148900                                                                          
149000     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
149100             WS-NUMBER-OF-SEGMENTS + 1                                    
149200                                                                          
149300     IF DIST42-EJ-EU-MIC OR                                               
149400        DIST42-OVR-MIC                                                    
149500                                                                          
149600       MOVE WC-A02-ETT             TO A02-TRANS-TYPE                      
149700                                                                          
149800       WRITE UT2-A02-POST        FROM A02-AREA                            
149900       MOVE  A02-AREA(1:3)       TO EDI-IDPTYP                            
150000       PERFORM S22-POSTSUM-UTPOST                                         
150100                                                                          
150200     END-IF                                                               
150300     .                                                                    
150400     EJECT                                                                
150500 EC-SKRIV-A03-POST SECTION.                                               
150600     MOVE 'EC-SKRIV-A03       '    TO WS-SEKTION                          
150700                                                                          
150800     MOVE WS-STRECK                TO A03-SLINE                           
150900     MOVE WC-A03-IDPTYP            TO A03-IDPTYP                          
151000                                                                          
151101*    MOVE IN-TU2-KDORSAK           TO A03-KDORSAK                         
151201*    MOVE IN-TU2-IDREFDDS          TO A03-IDREFDDS                        
151301                                                                          
151401     IF IN-TU2-IDSTATNR = ZERO                                            
151501       MOVE '087089997'            TO WS-IDSTATNR                         
151601     ELSE                                                                 
151701       MOVE IN-TU2-IDSTATNR        TO WS-IDSTATNR                         
151801     END-IF                                                               
151901     MOVE WS-IDSTATNR(2:8)         TO WS-IDSTATNR-ENBART                  
152001                                                                          
152101     MOVE WS-IDSTATNR-ENBART       TO STATNR-IDSTATNR                     
152201     IF STATNR-TILLAGG                                                    
152301       MOVE WC-A03-4039            TO WS-TILLAGG                          
152401       MOVE WS-IDSTATNR-TILLAGG    TO A03-CUSTOMS-TARIFF-NO               
152501     ELSE                                                                 
152601       MOVE WS-IDSTATNR-ENBART     TO A03-CUSTOMS-TARIFF-NO               
152701     END-IF                                                               
152801                                                                          
152901     WRITE UT-A03-POST             FROM A03-AREA                          
153001     MOVE A03-AREA(1:3)            TO EDI-IDPTYP                          
153101     PERFORM S21-POSTSUM-UTPOST                                           
153201                                                                          
153301     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
153401             WS-NUMBER-OF-SEGMENTS + 1                                    
153501                                                                          
153601     IF DIST42-EJ-EU-MIC OR                                               
153701        DIST42-OVR-MIC                                                    
153801                                                                          
153901       WRITE UT2-A03-POST        FROM A03-AREA                            
154001       MOVE  A03-AREA(1:3)       TO EDI-IDPTYP                            
154101       PERFORM S22-POSTSUM-UTPOST                                         
154201                                                                          
154301     END-IF                                                               
154401     .                                                                    
154501     EJECT                                                                
154601 ED-SKRIV-A04-POST SECTION.                                               
154701     MOVE 'ED-SKRIV-A04       '    TO WS-SEKTION                          
154801                                                                          
154901     MOVE WS-STRECK                TO A04-SLINE                           
155001     MOVE WC-A04-IDPTYP            TO A04-IDPTYP                          
155101                                                                          
155201     MOVE IN-TU2-IDSTATNR          TO STATNR-IDSTATNR                     
155301     IF STATNR-STYCK                                                      
155401       MOVE WC-A04-OTHER-Q-1       TO A04-OTHER-QUANTITY                  
155501     ELSE                                                                 
155601       MOVE WC-A04-OTHER-Q-0       TO A04-OTHER-QUANTITY                  
155701     END-IF                                                               
155801                                                                          
155901     WRITE UT-A04-POST             FROM A04-AREA                          
156001     MOVE A04-AREA(1:3)            TO EDI-IDPTYP                          
156101     PERFORM S21-POSTSUM-UTPOST                                           
156201                                                                          
156301     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
156401             WS-NUMBER-OF-SEGMENTS + 1                                    
156501                                                                          
156601     IF DIST42-EJ-EU-MIC OR                                               
156701        DIST42-OVR-MIC                                                    
156801                                                                          
156901       MOVE IN-TU2-IDKOLLI-MIC       TO A04-IDKOLLI-MIC                   
157001                                                                          
157101       EVALUATE TRUE                                                      
157201       WHEN IN-TU2-KDEMBTYP = 1 OR 6                                      
157301         MOVE '4G'                   TO A04-BEEMBTYP                      
157401       WHEN IN-TU2-KDEMBTYP = 2                                           
157501         MOVE 'CT'                   TO A04-BEEMBTYP                      
157601       WHEN IN-TU2-KDEMBTYP = 3                                           
157701         MOVE 'BE'                   TO A04-BEEMBTYP                      
157801       WHEN IN-TU2-KDEMBTYP = 4                                           
157901         MOVE 'CR'                   TO A04-BEEMBTYP                      
158001       WHEN IN-TU2-KDEMBTYP = 5                                           
158101         MOVE 'PP'                   TO A04-BEEMBTYP                      
158201       WHEN IN-TU2-KDEMBTYP = 7                                           
158301         MOVE 'PX'                   TO A04-BEEMBTYP                      
158401       WHEN IN-TU2-KDEMBTYP = 8                                           
158501         MOVE '4H'                   TO A04-BEEMBTYP                      
158601       WHEN OTHER                                                         
158701         MOVE '4G'                   TO A04-BEEMBTYP                      
158801       END-EVALUATE                                                       
158901                                                                          
159001       MOVE IN-TU2-VKORDBTO-KOLLI    TO WS-VKORDBTO-NUM                   
159101       MOVE WS-VKORDBTO-HEL          TO WS-B-HEL                          
159201       MOVE WS-VKORDBTO-DEC          TO WS-B-DEC                          
159301       MOVE WS-PUNKT                 TO WS-B-PUNKT                        
159401       MOVE WS-VKORDBTO-PUNKT        TO A04-VKORDBTO-KOLLI                
159501                                                                          
159601       MOVE IN-TU2-VLORDBTO-KOLLI    TO WS-VLORDBTO-NUM                   
159701       MOVE WS-VLORDBTO-HEL          TO WS-L-HEL                          
159801       MOVE WS-VLORDBTO-DEC          TO WS-L-DEC                          
159901       MOVE WS-PUNKT                 TO WS-L-PUNKT                        
160001       MOVE WS-VLORDBTO-PUNKT        TO A04-VLORDBTO-KOLLI                
160101                                                                          
160201       WRITE UT2-A04-POST        FROM A04-AREA                            
160301       MOVE  A04-AREA(1:3)       TO EDI-IDPTYP                            
160401       PERFORM S22-POSTSUM-UTPOST                                         
160501                                                                          
160601     END-IF                                                               
160701     .                                                                    
160801     EJECT                                                                
160901 EE-SKRIV-A06-POST SECTION.                                               
161001     MOVE 'EE-SKRIV-A06       '    TO WS-SEKTION                          
161101                                                                          
161201     MOVE WS-STRECK                TO A06-SLINE                           
161301     MOVE WC-A06-IDPTYP            TO A06-IDPTYP                          
161401                                                                          
161501     MOVE SPAR-TU1-IDDISTR         TO TEST-IDDISTR                        
161601     MOVE WC-A06-PROD-1000         TO A06-ED-PROC                         
161701                                                                          
161801     WRITE UT-A06-POST             FROM A06-AREA                          
161901     MOVE A06-AREA(1:3)            TO EDI-IDPTYP                          
162001     PERFORM S21-POSTSUM-UTPOST                                           
162101                                                                          
162201     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
162301             WS-NUMBER-OF-SEGMENTS + 1                                    
162401                                                                          
162501     IF DIST42-EJ-EU-MIC OR                                               
162601        DIST42-OVR-MIC                                                    
162701                                                                          
162801       MOVE IN-TU2-IDPARTNER       TO A06-IDPARTNER                       
162901                                                                          
163001       WRITE UT2-A06-POST        FROM A06-AREA                            
163101       MOVE  A06-AREA(1:3)       TO EDI-IDPTYP                            
163201       PERFORM S22-POSTSUM-UTPOST                                         
163301                                                                          
163401     END-IF                                                               
163501     .                                                                    
163601     EJECT                                                                
163701 F-SKAPA-FORP-POSTER SECTION.                                             
163801     MOVE 'F-SKAPA-FORP       '    TO WS-SEKTION                          
163901                                                                          
164001     INITIALIZE P01-AREA                                                  
164101                P05-AREA                                                  
164201                P08-AREA                                                  
164301                                                                          
164401     PERFORM FA-SKRIV-P01-POST                                            
164501     PERFORM FB-SKRIV-P05-POST                                            
164601     PERFORM FC-SKRIV-P08-POST                                            
164701     .                                                                    
164801     EJECT                                                                
164901 FA-SKRIV-P01-POST SECTION.                                               
165001     MOVE 'FA-SKRIV-P01       '    TO WS-SEKTION                          
165101                                                                          
165201     MOVE WS-STRECK                TO P01-SLINE                           
165301     MOVE WC-P01-IDPTYP            TO P01-IDPTYP                          
165401                                                                          
165501     MOVE SPAR-TU1-IDFAKT          TO P01-NUMBER                          
165601     MOVE SPAR-TU1-KVKOLLI-FAKT    TO P01-NUMBER-OF-PACKAGES              
165701     MOVE WC-P01-TYPE-OF-PACKAGES  TO P01-TYPE-OF-PACKAGES                
165801                                                                          
165901     WRITE UT-P01-POST             FROM P01-AREA                          
166001     MOVE P01-AREA(1:3)            TO EDI-IDPTYP                          
166101     PERFORM S21-POSTSUM-UTPOST                                           
166201                                                                          
166301     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
166401             WS-NUMBER-OF-SEGMENTS + 1                                    
166501                                                                          
166601     MOVE SPAR-TU1-IDDISTR       TO TEST-IDDISTR                          
166701     IF DIST42-EJ-EU-MIC OR                                               
166801        DIST42-OVR-MIC                                                    
166901                                                                          
167001       WRITE UT2-P01-POST        FROM P01-AREA                            
167101       MOVE  P01-AREA(1:3)       TO EDI-IDPTYP                            
167201       PERFORM S22-POSTSUM-UTPOST                                         
167301     END-IF                                                               
167401     .                                                                    
167501     EJECT                                                                
167601 FB-SKRIV-P05-POST SECTION.                                               
167701     MOVE 'FB-SKRIV-P05       '    TO WS-SEKTION                          
167801                                                                          
167901     MOVE WS-STRECK                TO P05-SLINE                           
168001     MOVE WC-P05-IDPTYP            TO P05-IDPTYP                          
168101                                                                          
168201     MOVE SPAR-TU1-VKRAD-NTO-KG-FAKT                                      
168301                                   TO WS-VKRADNTO-NUM                     
168401     MOVE WS-VKRADNTO-HEL          TO WS-V-HEL                            
168501     MOVE WS-VKRADNTO-DEC          TO WS-V-DEC                            
168601     MOVE WS-PUNKT                 TO WS-V-PUNKT                          
168701     MOVE WS-VKRADNTO-PUNKT        TO P05-VKORDNTO-FAKT                   
168801                                                                          
168901     WRITE UT-P05-POST             FROM P05-AREA                          
169001     MOVE P05-AREA(1:3)            TO EDI-IDPTYP                          
169101     PERFORM S21-POSTSUM-UTPOST                                           
169201                                                                          
169301     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
169401             WS-NUMBER-OF-SEGMENTS + 1                                    
169501                                                                          
169601     IF DIST42-EJ-EU-MIC OR                                               
169701        DIST42-OVR-MIC                                                    
169801                                                                          
169901       MOVE SPAR-TU1-VKORDBTO-FAKT TO WS-VKORDBTO-NUM                     
170001       MOVE WS-VKORDBTO-HEL        TO WS-B-HEL                            
170101       MOVE WS-VKORDBTO-DEC        TO WS-B-DEC                            
170201       MOVE WS-PUNKT               TO WS-B-PUNKT                          
170301       MOVE WS-VKORDBTO-PUNKT      TO P05-VKORDBTO-FAKT                   
170401                                                                          
170501       WRITE UT2-P05-POST        FROM P05-AREA                            
170601       MOVE  P05-AREA(1:3)       TO EDI-IDPTYP                            
170701       PERFORM S22-POSTSUM-UTPOST                                         
170801     END-IF                                                               
170901     .                                                                    
171001     EJECT                                                                
171101 FC-SKRIV-P08-POST SECTION.                                               
171201     MOVE 'FC-SKRIV-P08       '    TO WS-SEKTION                          
171301                                                                          
171401     MOVE WS-STRECK                TO P08-SLINE                           
171501     MOVE WC-P08-IDPTYP            TO P08-IDPTYP                          
171601                                                                          
171701     MOVE SPAR-TU1-IDLBBET         TO P08-CONTAINER-NO                    
171801                                                                          
171901     WRITE UT-P08-POST             FROM P08-AREA                          
172001     MOVE P08-AREA(1:3)            TO EDI-IDPTYP                          
172101     PERFORM S21-POSTSUM-UTPOST                                           
172201                                                                          
172301     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
172401             WS-NUMBER-OF-SEGMENTS + 1                                    
172501                                                                          
172601     IF DIST42-EJ-EU-MIC OR                                               
172701        DIST42-OVR-MIC                                                    
172801                                                                          
172901       IF SPAR-TU1-FLCONTAIN = SPACE OR NEJ                               
173001         MOVE SPACE              TO P08-CONTAINER-NO                      
173101       END-IF                                                             
173201                                                                          
173301       WRITE UT2-P08-POST        FROM P08-AREA                            
173401       MOVE  P08-AREA(1:3)       TO EDI-IDPTYP                            
173501       PERFORM S22-POSTSUM-UTPOST                                         
173601                                                                          
173701     END-IF                                                               
173801     .                                                                    
173901     EJECT                                                                
174001 G-SKRIV-E01-POST SECTION.                                                
174101     MOVE 'G-SKRIV-E01        '    TO WS-SEKTION                          
174201                                                                          
174301     INITIALIZE E01-AREA                                                  
174401                                                                          
174501     MOVE WS-STRECK                TO E01-SLINE                           
174601     MOVE WC-E01-IDPTYP            TO E01-IDPTYP                          
174701                                                                          
174801     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
174901             WS-NUMBER-OF-SEGMENTS + 1                                    
175001                                                                          
175101     MOVE WS-NUMBER-OF-SEGMENTS    TO E01-ANTAL                           
175201                                                                          
175301     WRITE UT-E01-POST             FROM E01-AREA                          
175401     MOVE E01-AREA(1:3)            TO EDI-IDPTYP                          
175501     PERFORM S21-POSTSUM-UTPOST                                           
175601                                                                          
175701     IF DIST42-EJ-EU-MIC OR                                               
175801        DIST42-OVR-MIC                                                    
175901                                                                          
176001       WRITE UT2-E01-POST        FROM E01-AREA                            
176101       MOVE  E01-AREA(1:3)       TO EDI-IDPTYP                            
176201       PERFORM S22-POSTSUM-UTPOST                                         
176301                                                                          
176401     END-IF                                                               
176501                                                                          
176601     MOVE ZERO                     TO WS-NUMBER-OF-SEGMENTS               
176701     .                                                                    
176801     EJECT                                                                
176901 S01-LAES-W47680  SECTION.                                                
177001     MOVE 'S01-LAES-W47680    '    TO WS-SEKTION                          
177101                                                                          
177201     READ W47680 INTO IN-AREA                                             
177301     AT END                                                               
177401        MOVE HIGH-VALUE TO IN-AREA                                        
177501        SET W47680-EOF TO TRUE                                            
177601                                                                          
177701     NOT AT END                                                           
177801        MOVE 'W47680' TO POSTSUM-FDNAMN                                   
177901        MOVE 'W47681D1' TO POSTSUM-DDNAMN2                                
178001        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
178101        CALL POSTSUM USING POSTSUM-PARM                                   
178201     END-READ                                                             
178301     .                                                                    
178401     EJECT                                                                
178501 S10-TA-FRAM-TRPSATT SECTION.                                             
178601     MOVE 'S10-TA-FRAM-TRPSATT'    TO WS-SEKTION                          
178701                                                                          
178801     IF IN-TU1-IDDISTR > 799 AND < 900                                    
178901       IF IN-TU1-KDFRAKT = +17                                            
179001         MOVE  4 TO WS-KDTRPSATT                                          
179101       ELSE                                                               
179201         MOVE  3 TO WS-KDTRPSATT                                          
179301       END-IF                                                             
179401     ELSE                                                                 
179501       IF IN-TU1-KDFRAKT = +21 OR +22 OR +23 OR +24 OR +25 OR             
179601                           +26 OR +27 OR +28 OR                           
179701                           +41 OR +42 OR +43 OR +44 OR +45                
179801         MOVE 1  TO WS-KDTRPSATT                                          
179901       ELSE                                                               
180001         EVALUATE TRUE                                                    
180101                                                                          
180201            WHEN IN-TU1-KDFRAKT = +01 OR +05 OR +06 OR +07 OR             
180301                                  +08 OR +09 OR +10 OR +13 OR             
180401                                  +31 OR +32 OR +33 OR                    
180501                                  +34 OR +35 OR +37 OR                    
180601                                  +47 OR                                  
180701                                  +99                                     
180801                 MOVE 3  TO WS-KDTRPSATT                                  
180901                                                                          
181001            WHEN IN-TU1-KDFRAKT = +14 OR +16 OR +17 OR +18 OR             
181101                                  +19 OR +50 OR +61                       
181201                 MOVE 4  TO WS-KDTRPSATT                                  
181301                                                                          
181401            WHEN OTHER                                                    
181501                 MOVE 3  TO WS-KDTRPSATT                                  
181601         END-EVALUATE                                                     
181701       END-IF                                                             
181801     END-IF                                                               
181901     .                                                                    
182001 S11-TA-FRAM-TRPSATT SECTION.                                             
182101     MOVE 'S11-TA-FRAM-TRPSATT'    TO WS-SEKTION                          
182201                                                                          
182301     IF IN-TU1-KDTRPTYP = 1                                               
182401       MOVE 1    TO WS-KDTRPSATT                                          
182501     END-IF                                                               
182601                                                                          
182701     IF IN-TU1-KDTRPTYP = 2                                               
182801       MOVE 2    TO WS-KDTRPSATT                                          
182901     END-IF                                                               
183001                                                                          
183101     IF IN-TU1-KDTRPTYP = 3                                               
183201       MOVE 3    TO WS-KDTRPSATT                                          
183301     END-IF                                                               
183401                                                                          
183501     IF IN-TU1-KDTRPTYP = 4                                               
183601       MOVE 4    TO WS-KDTRPSATT                                          
183701     END-IF                                                               
183801     .                                                                    
183901 S21-POSTSUM-UTPOST SECTION.                                              
184001     MOVE 'S21-POSTSUM-UTPOST '    TO WS-SEKTION                          
184101                                                                          
184201     MOVE EDI-IDPTYP TO POSTSUM-TRANSTYP                                  
184301     MOVE 'W47681' TO POSTSUM-FDNAMN                                      
184401     MOVE 'W47681D2' TO POSTSUM-DDNAMN2                                   
184501     CALL POSTSUM USING POSTSUM-PARM                                      
184601     .                                                                    
184701     EJECT                                                                
184801                                                                          
184901 S22-POSTSUM-UTPOST  SECTION.                                             
185001     MOVE 'S22-POSTSUM-UTPOST '    TO WS-SEKTION                          
185101                                                                          
185201     MOVE EDI-IDPTYP TO POSTSUM-TRANSTYP                                  
185301     MOVE 'W47681X' TO POSTSUM-FDNAMN                                     
185401     MOVE 'W47681D3' TO POSTSUM-DDNAMN2                                   
185501     CALL POSTSUM USING POSTSUM-PARM                                      
185601     .                                                                    
185701     EJECT                                                                
185801                                                                          
185901 Z-FINIT SECTION.                                                         
186001     MOVE 'Z-FINIT            '    TO WS-SEKTION                          
186101                                                                          
186201     CLOSE W47680                                                         
186301           W47681                                                         
186401           W47681X                                                        
186501     SKIP2                                                                
186601     MOVE 'S' TO POSTSUM-OPKOD                                            
186701     CALL POSTSUM USING POSTSUM-PARM                                      
186801     .                                                                    
187000     EJECT                                                                
