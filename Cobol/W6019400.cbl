000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6019400.                                                
000300 AUTHOR.         GUNNAR LARSSON IDK.                                      
000400 DATE-WRITTEN.   92/04/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET ÄR EN BAKGRUNDS-MPP SOM                               
000900*        HANTERAR UTSKRIFT AV FLAGGOR.                                    
001000*                                                                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W6T194U                                             
001400*        MID:         W6I19401                                            
001500*                                                                         
001600*    UTDATA:                                                              
001700*        UTSKRIVNA FLAGGOR (VIA W006PRC1 och W006PRS1)                    
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200                                                                          
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700                                                                          
002800 77  IDPGM                       PIC X(08)   VALUE 'W6019400'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003101 01  FILLER                      PIC X(8)    VALUE 'FELTEXT:'.            
003110 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
003700     88  EGEN-MID                            VALUE '6194'.                
003800     88  GODK-MID                            VALUE '6194'                 
003900                                                   '6173'.                
004000                                                                          
004100 77  WS-IDPRTLST                 PIC X(8)    VALUE SPACE.                 
004200                                                                          
004300 01  FIX.                                                                 
004400   03  FIXA.                                                              
004500     05  FIX1                    PIC X       VALUE SPACE.                 
004600     05  FIXB                    PIC X(15)   VALUE SPACE.                 
004700                                                                          
004800 01  FILLER                      PIC X(8)    VALUE 'IX-*****'.            
004900 01  IX-INDEXVARIABLER.                                                   
005000*     -- POST INOM MID                                                    
005100   03  IX-POST                   PIC S9(9)   VALUE ZERO COMP SYNC.        
005200   03  FLAGG-IX                  PIC S9(9)   VALUE ZERO COMP SYNC.        
005300   03  FLAGG-IX-NOVA             PIC S9(9)   VALUE ZERO COMP SYNC.        
005400   03  FLAGG-IX-VCOM             PIC S9(9)   VALUE ZERO COMP SYNC.        
005500   03  ZEBRA-IX                  PIC S9(9)   VALUE ZERO COMP SYNC.        
005600*     -- MAX ANTAL POSTER I MID                                           
005700   03    MAX-IX-POST             PIC S9(9)   VALUE +15  COMP SYNC.        
005800*     -- MAX ANTAL RADER PER FLAGGA                                       
005900   03    MAX-IX                  PIC S9(9)   VALUE +41  COMP SYNC.        
006000   03    MAX-IX-NOVA             PIC S9(9)   VALUE +41  COMP SYNC.        
006100   03    MAX-IX-VCOM             PIC S9(9)   VALUE +14  COMP SYNC.        
006200   03    MAX-IX-ZEBRA            PIC S9(9)   VALUE +38  COMP SYNC.        
006300                                                                          
006400 01      LIST1-ARTIKEL-RAD       PIC X(132).                              
006500                                                                          
006600     EJECT                                                                
006700*    --- KOLLI-FLAGGA-LISTA ARBETSFÄLT                                    
006800 01      FILLER                  PIC X(16)                                
006900                                 VALUE 'FLAGG-TAB-VCOM'.                  
007000 01      FLAGG-TAB-VCOM.                                                  
007100   03    FILLER                  PIC X(80)   VALUE                        
007200         '!M "FLAGGA"                                          '.         
007300   03    FILLER.                                                          
007400      05 FLAGG-TAB-IDARTNR-VCOM  PIC X(8).                                
007500      05 FILLER                  PIC X(72)   VALUE SPACE.                 
007600   03    FILLER.                                                          
007700      05 FLAGG-TAB-KVINLART-VCOM PIC X(6).                                
007800      05 FILLER                  PIC X(74)   VALUE SPACE.                 
007900   03    FILLER.                                                          
008000      05 FLAGG-TAB-IDLEVNR-VCOM  PIC X(5).                                
008100      05 FILLER                  PIC X(75)   VALUE SPACE.                 
008200   03    FILLER.                                                          
008300      05 FLAGG-TAB-IDOKOLLI-VCOM PIC X(9).                                
008400      05 FILLER                  PIC X(71)   VALUE SPACE.                 
008500   03    FILLER.                                                          
008600      05 FLAGG-TAB-TIAVIDAT-VCOM PIC 9(6).                                
008700      05 FILLER                  PIC X(74)   VALUE SPACE.                 
008800   03    FILLER.                                                          
008900      05 FLAGG-TAB-VKARTNTO-VCOM PIC Z(5).                                
009000      05 FILLER                  PIC X(75)   VALUE SPACE.                 
009100   03    FILLER.                                                          
009200      05 FLAGG-TAB-IDLOPNRM-VCOM PIC Z(8).                                
009300      05 FILLER                  PIC X(72)   VALUE SPACE.                 
009400   03    FILLER.                                                          
009500      05 FLAGG-TAB-ADLAGOMR-VCOM PIC Z(2).                                
009600      05 FILLER                  PIC X(78)   VALUE SPACE.                 
009700   03    FILLER.                                                          
009800      05 FLAGG-TAB-ADGANG-VCOM   PIC Z(2).                                
009900      05 FILLER                  PIC X(78)   VALUE SPACE.                 
010000   03    FILLER.                                                          
010100      05 FLAGG-TAB-ADPLATS-VCOM  PIC Z(5).                                
010200      05 FILLER                  PIC X(75)   VALUE SPACE.                 
010300   03    FILLER.                                                          
010400      05 FLAGG-TAB-KDSORT-VCOM   PIC X(2).                                
010500      05 FILLER                  PIC X(78)   VALUE SPACE.                 
010600   03    FILLER                  PIC X(80)   VALUE                        
010700         '!P                                                   '.         
010800   03    FILLER                  PIC X(80)   VALUE                        
010900         '!R                                                   '.         
011000 01  FILLER REDEFINES FLAGG-TAB-VCOM.                                     
011100   03  FILLER OCCURS 14.                                                  
011200     05  FLAGG-RAD-VCOM          PIC X(80).                               
011300                                                                          
011400     EJECT                                                                
011500*    --- KOLLI-FLAGGA-LISTA ARBETSFÄLT                                    
011600 01      FILLER                  PIC X(16)   VALUE 'FLAGG-TAB'.           
011700 01      FLAGG-TAB.                                                       
011800   03    FILLER                  PIC X(80)   VALUE                        
011900         '!C                                                   '.         
012000   03    FILLER                  PIC X(80)   VALUE                        
012100         '!C                                                   '.         
012200   03    FILLER                  PIC X(80)   VALUE                        
012300         '!K125                                                '.         
012400   03    FILLER                  PIC X(80)   VALUE                        
012500         '!F T S 930 2135 L 1 1 3 "Artikelnummer (P)"          '.         
012600   03    FILLER                  PIC X(80)   VALUE                        
012700         '!F T S 725 2130 L 1 1 3 "Antal (Q)"                  '.         
012800   03    FILLER.                                                          
012900      05 FILLER                  PIC X(25)   VALUE                        
013000         '!F T S 685 1300 L 2 2 5 "'.                                     
013100      05 FLAGG-TAB-KDSORT        PIC X(2).                                
013200      05 FILLER                  PIC X(53)   VALUE                        
013300         '"                        '.                                     
013400   03    FILLER                  PIC X(80)   VALUE                        
013500         '!F T S 450 2135 L 1 1 3 "Leverantörsnummer (V)"      '.         
013600   03    FILLER                  PIC X(80)   VALUE                        
013700         '!F T S 240 2130 L 1 1 3 "Kollilöpnr (S)"             '.         
013800   03    FILLER.                                                          
013900      05 FILLER                  PIC X(24)   VALUE                        
014000         '!F T S 980 200 R 8 6 6 "'.                                      
014100      05 FLAGG-TAB-IDARTNR       PIC X(8).                                
014200      05 FILLER                  PIC X(48)   VALUE                        
014300         '"                        '.                                     
014400   03    FILLER.                                                          
014500      05 FILLER                  PIC X(25)   VALUE                        
014600         '!F T S 630 1400 R 5 4 5 "'.                                     
014700      05 FLAGG-TAB-KVINLART      PIC X(6).                                
014800      05 FILLER                  PIC X(49)   VALUE                        
014900         '"                        '.                                     
015000   03    FILLER.                                                          
015100      05 FILLER                  PIC X(25)   VALUE                        
015200         '!F T S 425 1400 R 2 2 5 "'.                                     
015300      05 FLAGG-TAB-IDLEVNR       PIC X(5).                                
015400      05 FILLER                  PIC X(50)   VALUE                        
015500         '"                        '.                                     
015600   03    FILLER.                                                          
015700      05 FILLER                  PIC X(25)   VALUE                        
015800         '!F T S 185 1250 R 3 2 5 "'.                                     
015900      05 FLAGG-TAB-IDOKOLLI      PIC X(9).                                
016000      05 FILLER                  PIC X(46)   VALUE                        
016100         '"                        '.                                     
016200   03    FILLER                  PIC X(80)   VALUE                        
016300         '!F T S 20 2130 L 1 1 3 "VCAS Göteborg"               '.         
016400   03    FILLER                  PIC X(80)   VALUE                        
016500         '!F T S 898 1082 L 1 1 3 "Datum"                      '.         
016600   03    FILLER                  PIC X(80)   VALUE                        
016700         '!F T S 750 1082 L 1 1 3 "Vikt netto"                 '.         
016800   03    FILLER                  PIC X(80)   VALUE                        
016900         '!F T S 603 1080 L 1 1 3 "Partinr"                    '.         
017000   03    FILLER                  PIC X(80)   VALUE                        
017100         '!F T S 450 1080 L 1 1 3 "Plats"                      '.         
017200   03    FILLER                  PIC X(80)   VALUE                        
017300         '!F T S 894 685 L 1 1 3 "Område"                      '.         
017400   03    FILLER                  PIC X(80)   VALUE                        
017500         '!F T S 894 397 L 1 1 3 "Gång"                        '.         
017600   03    FILLER.                                                          
017700      05 FILLER                  PIC X(25)   VALUE                        
017800         '!F T S 793 1055 L 4 2 5 "'.                                     
017900      05 FLAGG-TAB-TIAVIDAT      PIC 9(6).                                
018000      05 FILLER                  PIC X(49)   VALUE                        
018100         '"                        '.                                     
018200   03    FILLER.                                                          
018300      05 FILLER                  PIC X(24)   VALUE                        
018400         '!F T S 645 725 R 4 3 5 "'.                                      
018500      05 FLAGG-TAB-VKARTNTO      PIC Z(5).                                
018600      05 FILLER                  PIC X(51)   VALUE                        
018700         '"                        '.                                     
018800   03    FILLER.                                                          
018900      05 FILLER                  PIC X(24)   VALUE                        
019000         '!F T S 498 725 R 2 2 5 "'.                                      
019100      05 FLAGG-TAB-IDLOPNRM      PIC Z(8).                                
019200      05 FILLER                  PIC X(48)   VALUE                        
019300         '"                        '.                                     
019400   03    FILLER.                                                          
019500      05 FILLER                  PIC X(25)   VALUE                        
019600         '!F T S 500 675 L 13 4 5 "'.                                     
019700      05 FLAGG-TAB-ADLAGOMR      PIC Z(2).                                
019800      05 FILLER                  PIC X(53)   VALUE                        
019900         '"                        '.                                     
020000   03    FILLER.                                                          
020100      05 FILLER                  PIC X(25)   VALUE                        
020200         '!F T S 500 395 L 8 4 5  "'.                                     
020300      05 FLAGG-TAB-ADGANG        PIC Z(2).                                
020400      05 FILLER                  PIC X(53)   VALUE                        
020500         '"                        '.                                     
020600   03    FILLER.                                                          
020700      05 FILLER                  PIC X(25)   VALUE                        
020800         '!F T S 105 200 R 14 8 5 "'.                                     
020900      05 FLAGG-TAB-ADPLATS       PIC Z(5).                                
021000      05 FILLER                  PIC X(50)   VALUE                        
021100         '"                        '.                                     
021200   03    FILLER.                                                          
021300      05 FILLER                  PIC X(29)   VALUE                        
021400         '!F C S 775 2110 L 130 3 12 "P'.                                 
021500      05 FLAGG-TAB-IDARTNR-STRK  PIC X(9).                                
021600      05 FILLER                  PIC X(42)   VALUE                        
021700         '                         '.                                     
021800   03    FILLER.                                                          
021900      05 FILLER                  PIC X(29)   VALUE                        
022000         '!F C S 495 2115 L 130 3 12 "Q'.                                 
022100      05 FLAGG-TAB-KVINLART-STRK  PIC X(7).                               
022200      05 FILLER                  PIC X(44)   VALUE                        
022300         '                         '.                                     
022400   03    FILLER.                                                          
022500      05 FILLER                  PIC X(29)   VALUE                        
022600         '!F C S 285 2110 L 130 3 12 "V'.                                 
022700      05 FLAGG-TAB-IDLEVNR-STRK  PIC X(6).                                
022800      05 FILLER                  PIC X(45)   VALUE                        
022900         '                         '.                                     
023000   03    FILLER.                                                          
023100      05 FILLER                  PIC X(29)   VALUE                        
023200         '!F C S 45 2110 L 130 3 12  "S'.                                 
023300      05 FLAGG-TAB-IDOKOLLI-STRK  PIC X(10).                              
023400      05 FILLER                  PIC X(41)   VALUE                        
023500         '                         '.                                     
023600   03    FILLER                  PIC X(80)   VALUE                        
023700         '!F B S 24 1105 L 904 5                               '.         
023800   03    FILLER                  PIC X(80)   VALUE                        
023900         '!F B S 925 1100 L 5 950                              '.         
024000   03    FILLER                  PIC X(80)   VALUE                        
024100         '!F B S 760 2140 L 5 1035                             '.         
024200   03    FILLER                  PIC X(80)   VALUE                        
024300         '!F B S 270 2140 L 5 1035                             '.         
024400   03    FILLER                  PIC X(80)   VALUE                        
024500         '!F B S 480 1100 L 5 950                              '.         
024600   03    FILLER                  PIC X(80)   VALUE                        
024700         '!F B S 485 705  L 440 5                              '.         
024800   03    FILLER                  PIC X(80)   VALUE                        
024900         '!F B S 830 422  L 95 50                              '.         
025000   03    FILLER                  PIC X(80)   VALUE                        
025100         '!F B S 775 1100 L 5 395                              '.         
025200   03    FILLER                  PIC X(80)   VALUE                        
025300         '!F B S 628 1100 L 5 395                              '.         
025400   03    FILLER                  PIC X(80)   VALUE                        
025500         '!F B S 480 2145 L 5 1040                             '.         
025600   03    FILLER                  PIC X(80)   VALUE                        
025700         '!P                                                   '.         
025800   03    FILLER                  PIC X(80)   VALUE                        
025900         '!R                                                   '.         
026000 01  FILLER REDEFINES FLAGG-TAB.                                          
026100   03  FILLER OCCURS 41.                                                  
026200     05  FLAGG-RAD               PIC X(80).                               
026300                                                                          
026400                                                                          
026500     EJECT                                                                
026510*    --- KOLLI-FLAGGA-LISTA NOVA ARBETSFÄLT                               
026520*    --- KOLLI-FLAGGA-LISTA NOVA ARBETSFÄLT                               
026600*    --- KOLLI-FLAGGA-LISTA-NOVA ARBETSFÄLT                               
026700 01      FILLER                  PIC X(08)   VALUE 'NOVA-TAB'.            
026800 01      NOVA-TAB.                                                        
026900   03    FILLER                  PIC X(80)   VALUE                        
027000         '!C                                                   '.         
027100   03    FILLER                  PIC X(80)   VALUE                        
027200         '!C                                                   '.         
027300   03    FILLER                  PIC X(80)   VALUE                        
027400         '!Y42 0                                               '.         
027500   03    FILLER                  PIC X(80)   VALUE                        
027600         '!F T E 1060   2 L 1 1 3 "Artikelnummer (P)"          '.         
027700   03    FILLER                  PIC X(80)   VALUE                        
027800         '!F T E  870   2 L 1 1 3 "ANTAL (Q)"                  '.         
027900   03    FILLER.                                                          
028000      05 FILLER                  PIC X(25)   VALUE                        
028100         '!F T E 800 1000 R 2 2 5 "'.                                     
028200      05 NOVA-TAB-KDSORT        PIC X(2).                                 
028300      05 FILLER                  PIC X(53)   VALUE                        
028400         '"                        '.                                     
028500   03    FILLER                  PIC X(80)   VALUE                        
028600         '!F T E  585    2 L 1 1 3 "Leverantörsnummer (V)"     '.         
028700   03    FILLER                  PIC X(80)   VALUE                        
028800         '!F T E  360    2 L 1 1 3 "Kollilöpnr (S)"            '.         
028900   03    FILLER.                                                          
029000      05 FILLER                  PIC X(26)   VALUE                        
029100         '!F T E 1140 1900 R 8 6 6 "'.                                    
029200      05 NOVA-TAB-IDARTNR-GRP.                                            
029201        07 NOVA-TAB-IDARTNR      PIC X(9).                                
029210      05 IDARTNR-Z-GRP REDEFINES NOVA-TAB-IDARTNR-GRP.                    
029220        07 NOVA-TAB-IDARTNRZ     PIC Z(9).                                
029300      05 FILLER                  PIC X(45)   VALUE                        
029400         '"                        '.                                     
029500   03    FILLER.                                                          
029600      05 FILLER                  PIC X(26)   VALUE                        
029700         '!F T E  770  300 L 5 4 5 "'.                                    
029810      05 NOVA-TAB-KVINLART       PIC X(6).                                
029900      05 FILLER                  PIC X(48)   VALUE                        
030000         '"                        '.                                     
030100   03    FILLER.                                                          
030200      05 FILLER                  PIC X(26)   VALUE                        
030300         '!F T E  550  800 R 2 2 5 "'.                                    
030400      05 NOVA-TAB-IDLEVNR        PIC X(5).                                
030500      05 FILLER                  PIC X(49)   VALUE                        
030600         '"                        '.                                     
030700   03    FILLER.                                                          
030800      05 FILLER                  PIC X(26)   VALUE                        
030900         '!F T E  300  800 R 3 2 5 "'.                                    
031020      05 NOVA-TAB-IDOKOLLI       PIC Z(9).                                
031100      05 FILLER                  PIC X(45)   VALUE                        
031200         '"                        '.                                     
031300   03    FILLER                  PIC X(80)   VALUE                        
031400       '!F T E  120    2 L 1 1 3 "VCAS Göteborg"             '.           
031500   03    FILLER                  PIC X(80)   VALUE                        
031600       '!F T E 1070 1070 L 1 1 3 "Datum"                     '.           
031700   03    FILLER                  PIC X(80)   VALUE                        
031800       '!F T E  930 1070 L 1 1 3 "Vikt netto"                '.           
031900   03    FILLER                  PIC X(80)   VALUE                        
032000       '!F T E  780 1070 L 1 1 3 "Partinr"                   '.           
032100   03    FILLER                  PIC X(80)   VALUE                        
032200       '!F T E  630 1070 L 1 1 3 "Plats"                      '.          
032300   03    FILLER                  PIC X(80)   VALUE                        
032400       '!F T E 1070 1460 L 1 1 3 "Område"                     '.          
032500   03    FILLER                  PIC X(80)   VALUE                        
032600       '!F T E 1070 1760 L 1 1 3 "Gång"                       '.          
032700   03    FILLER.                                                          
032800      05 FILLER                  PIC X(26)   VALUE                        
032900         '!F T E  970 1100 L 4 2 5 "'.                                    
033000      05 NOVA-TAB-TIAVIDAT       PIC 9(6).                                
033100      05 FILLER                  PIC X(48)   VALUE                        
033200         '"                        '.                                     
033300   03    FILLER.                                                          
033400      05 FILLER                  PIC X(26)   VALUE                        
033500         '!F T E 830 1400  R 4 3 5 "'.                                    
033600      05 NOVA-TAB-VKARTNTO       PIC Z(5).                                
033700      05 FILLER                  PIC X(49)   VALUE                        
033800         '"                        '.                                     
033900   03    FILLER.                                                          
034000      05 FILLER                  PIC X(26)   VALUE                        
034100         '!F T E 690 1400 R 2 2 5 "'.                                     
034200      05 NOVA-TAB-IDLOPNRM       PIC Z(8).                                
034300      05 FILLER                  PIC X(46)   VALUE                        
034400         '"                        '.                                     
034500   03    FILLER.                                                          
034600      05 FILLER                  PIC X(26)   VALUE                        
034700         '!F T E 690 1500 L 13 4 5 "'.                                    
034800      05 NOVA-TAB-ADLAGOMR       PIC Z(2).                                
034900      05 FILLER                  PIC X(52)   VALUE                        
035000         '"                        '.                                     
035100   03    FILLER.                                                          
035200      05 FILLER                  PIC X(26)   VALUE                        
035210         '!F T E 690 1800 L  8 4 5 "'.                                    
035400      05 NOVA-TAB-ADGANG         PIC Z(2).                                
035500      05 FILLER                  PIC X(52)   VALUE                        
035600         '"                        '.                                     
035700   03    FILLER.                                                          
035800      05 FILLER                  PIC X(27)   VALUE                        
035900         '!F T E 250 1940 R 14 8 5 "'.                                    
036000      05 NOVA-TAB-ADPLATS        PIC Z(5).                                
036100      05 FILLER                  PIC X(48)   VALUE                        
036200         '"                        '.                                     
036300   03    FILLER.                                                          
036400      05 FILLER                  PIC X(29)   VALUE                        
036500         '!F C E  910  10 L 130 3 12 "P'.                                 
036600      05 NOVA-TAB-IDARTNR-STRK   PIC X(9).                                
036700      05 FILLER                  PIC X(42)   VALUE                        
036800         '                         '.                                     
036900   03    FILLER.                                                          
037000      05 FILLER                  PIC X(29)   VALUE                        
037100         '!F C E  620  10 L 130 3 12 "Q'.                                 
037200      05 NOVA-TAB-KVINLART-STRK   PIC X(7).                               
037300      05 FILLER                  PIC X(44)   VALUE                        
037400         '                         '.                                     
037500   03    FILLER.                                                          
037600      05 FILLER                  PIC X(29)   VALUE                        
037700         '!F C E  400  10 L 130 3 12 "V'.                                 
037800      05 NOVA-TAB-IDLEVNR-STRK   PIC X(6).                                
037900      05 FILLER                  PIC X(45)   VALUE                        
038000         '                         '.                                     
038100   03    FILLER.                                                          
038200      05 FILLER                  PIC X(29)   VALUE                        
038300         '!F C E  150  10 L 130 3 12 "S'.                                 
038400      05 NOVA-TAB-IDOKOLLI-STRK   PIC X(10).                              
038500      05 FILLER                  PIC X(41)   VALUE                        
038600         '                         '.                                     
039100   03    FILLER                  PIC X(80)   VALUE                        
039200         '!F B E  900   2 L 5 1035                             '.         
039300   03    FILLER                  PIC X(80)   VALUE                        
039400         '!F B E  610   2 L 5 1035                             '.         
039410   03    FILLER                  PIC X(80)   VALUE                        
039420         '!F B E  390   2 L 5 1040                             '.         
039430   03    FILLER                  PIC X(80)   VALUE                        
039440         '!F B E  196 1040 L 904 5                             '.         
039450   03    FILLER                  PIC X(80)   VALUE                        
039460         '!F B E 1100 1040 L 5 950                             '.         
039500   03    FILLER                  PIC X(80)   VALUE                        
039600         '!F B E  660 1040 L 5 950                             '.         
039601   03    FILLER                  PIC X(80)   VALUE                        
039610         '!F B E  960 1040 L 5 395                             '.         
039620   03    FILLER                  PIC X(80)   VALUE                        
039630         '!F B E  810 1040 L 5 395                             '.         
039700   03    FILLER                  PIC X(80)   VALUE                        
039800         '!F B E  660 1445 L 440 5                            '.          
039900*  03    FILLER                  PIC X(80)   VALUE                        
040000*        '!F B E 1010 1710 L 95 50                             '.         
040100   03    FILLER                  PIC X(80)   VALUE                        
040800         '!P                                                   '.         
040900   03    FILLER                  PIC X(80)   VALUE                        
041000         '!R                                                   '.         
041100 01  FILLER REDEFINES NOVA-TAB.                                           
041200   03  FILLER OCCURS 41.                                                  
041300     05  FLAGG-RAD-NOVA          PIC X(80).                               
041400                                                                          
041410                                                                          
041500     EJECT                                                                
041600*    --- KOLLI-FLAGGA-LISTA ARBETSFÄLT ZEBRA      BORN                    
041610*    --- KOLLI-FLAGGA-LISTA ARBETSFÄLT ZEBRA      BORN                    
041620*    --- KOLLI-FLAGGA-LISTA ARBETSFÄLT ZEBRA      BORN                    
041630*    --- KOLLI-FLAGGA-LISTA ARBETSFÄLT ZEBRA      BORN                    
041700 01      FILLER                  PIC X(16)   VALUE 'ZEBRA-TAB'.           
041800 01      ZEBRA-TAB.                                                       
041900   03    FILLER                  PIC X(80)   VALUE                        
042000         '^XA^CF0^FWN^FS                               '.                 
042100   03    FILLER                  PIC X(80)   VALUE                        
042200         '^FO0082,0700^GB0768,1,05^FS                  '.                 
042300   03    FILLER                  PIC X(80)   VALUE                        
042400         '^FO0082,1050^GB0768,1,05^FS                  '.                 
042500   03    FILLER                  PIC X(80)   VALUE                        
042600         '^FO0082,1190^GB0768,1,05^FS                  '.                 
042700   03    FILLER                  PIC X(80)   VALUE                        
042800         '^FO0850,0550^GB1,0950,05^FS                  '.                 
042900   03    FILLER                  PIC X(80)   VALUE                        
043000         '^FO1150,0550^GB1,450,05^FS                   '.                 
043100   03    FILLER                  PIC X(80)   VALUE                        
043200         '^FO0850,0550^GB700,1,05^FS                   '.                 
043300   03    FILLER                  PIC X(80)   VALUE                        
043400         '^FO0850,0700^GB300,1,05^FS                   '.                 
043500   03    FILLER                  PIC X(80)   VALUE                        
043600         '^FO0850,0850^GB300,1,05^FS                   '.                 
043700   03    FILLER                  PIC X(80)   VALUE                        
043800         '^FO0850,1000^GB300,1,05^FS                   '.                 
043900   03    FILLER                  PIC X(80)   VALUE                        
044000         '^FO0082,0550^A0N,35,50^FDPart number (p) ^FS '.                 
044100   03    FILLER                  PIC X(80)   VALUE                        
044200         '^FO0082,0710^A0N,35,50^FDQuantity (q) ^FS    '.                 
044300   03    FILLER                  PIC X(80)   VALUE                        
044400         '^FO0700,0710^A0N,100,090^FDPCS^FS            '.                 
044500   03    FILLER                  PIC X(80)   VALUE                        
044600         '^FO0082,1060^A0N,35,50^FDSupplier (v) ^FS        '.             
044700   03    FILLER                  PIC X(80)   VALUE                        
044800         '^FO0082,1200^A0N,35,50^FDCase number (s) ^FS     '.             
044900   03    FILLER                  PIC X(80)   VALUE                        
045000         '^FO0082,1350^A0N,35,50^FD VOLVO ^FS    '.                       
045100   03    FILLER                  PIC X(80)   VALUE                        
045200         '^FO0875,0560^A0N,25,25^FDAdv. Date ^FS           '.             
045300   03    FILLER                  PIC X(80)   VALUE                        
045400         '^FO0875,0710^A0N,25,25^FDWeight net. (Lbs) ^FS   '.             
045500   03    FILLER                  PIC X(80)   VALUE                        
045600         '^FO0875,0860^A0N,25,25^FDSeq. no. ^FS            '.             
045700   03    FILLER                  PIC X(80)   VALUE                        
045800         '^FO0875,1030^A0N,35,50^FDLocation ^FS            '.             
045900   03    FILLER                  PIC X(80)   VALUE                        
046000         '^FO1160,0560^A0N,35,50^FDArea^FS                 '.             
046100   03    FILLER                  PIC X(80)   VALUE                        
046200         '^FO1400,0560^A0N,35,50^FDAisle^FS                '.             
046300   03    FILLER.                                                          
046400     05    FILLER                  PIC X(27)   VALUE                      
046500         '^FO0182,0150^A0N,375,325^FD'.                                   
046600     05    ZEBRA-IDARTNR           PIC X(8).                              
046700     05    FILLER                  PIC X(45)   VALUE                      
046800         '^FS                      '.                                     
046900   03    FILLER.                                                          
047000     05    FILLER                  PIC X(27)   VALUE                      
047100         '^FO0082,0750^A0N,250,200^FD'.                                   
047200     05    ZEBRA-KVINLART          PIC Z(6).                              
047300     05    FILLER                  PIC X(47)   VALUE                      
047400         '^FS                      '.                                     
047500   03    FILLER.                                                          
047600     05    FILLER                  PIC X(27)   VALUE                      
047700         '^FO0500,1060^A0N,050,050^FD'.                                   
047800     05    ZEBRA-IDLEVNR           PIC x(5).                              
047900     05    FILLER                  PIC X(48)   VALUE                      
048000         '^FS                      '.                                     
048100   03    FILLER.                                                          
048200     05    FILLER                  PIC X(27)   VALUE                      
048300         '^FO0410,1200^A0N,050,050^FD'.                                   
048400     05    ZEBRA-IDOKOLLI          PIC Z(9).                              
048500     05    FILLER                  PIC X(44)   VALUE                      
048600         '^FS                      '.                                     
048700   03    FILLER.                                                          
048800     05    FILLER                  PIC X(27)   VALUE                      
048900         '^FO0860,0600^A0N,075,075^FD'.                                   
049000     05    ZEBRA-TIAVIDAT          PIC 9(6)    VALUE ZERO.                
049100     05    FILLER                  PIC X(47)   VALUE                      
049200         '^FS                       '.                                    
049300   03    FILLER.                                                          
049400     05    FILLER                  PIC X(27)   VALUE                      
049500         '^FO0860,0750^A0N,075,075^FD'.                                   
049600     05    ZEBRA-VKARTNTO          PIC Z(5).                              
049700     05    FILLER                  PIC X(48)   VALUE                      
049800         '^FS                      '.                                     
049900   03    FILLER.                                                          
050000     05    FILLER                  PIC X(27)   VALUE                      
050100         '^FO0860,0910^A0N,075,075^FD'.                                   
050200     05    ZEBRA-IDLOPNRM          PIC Z(8).                              
050300     05    FILLER                  PIC X(45)   VALUE                      
050400         '^FS                      '.                                     
050500   03    FILLER.                                                          
050600     05    FILLER                  PIC X(27)   VALUE                      
050700         '^FO0850,1100^A0N,300,300^FD'.                                   
050800     05    ZEBRA-ADPLATS           PIC 9(5).                              
050900     05    FILLER                  PIC X(48)   VALUE                      
051000         '^FS                      '.                                     
051100   03    FILLER.                                                          
051200     05    FILLER                  PIC X(27)   VALUE                      
051300         '^FO1150,0600^A0N,250,200^FD'.                                   
051400     05    ZEBRA-ADLAGOMR          PIC Z(2).                              
051500     05    FILLER                  PIC X(51)   VALUE                      
051600         '^FS                      '.                                     
051700   03    FILLER.                                                          
051800     05    FILLER                  PIC X(27)   VALUE                      
051900         '^FO1400,0600^A0N,250,200^FD'.                                   
052000     05    ZEBRA-ADGANG            PIC Z(2).                              
052100     05    FILLER                  PIC X(51)   VALUE                      
052200         '^FS                      '.                                     
052300   03    FILLER                  PIC X(80)   VALUE                        
052400         '^BY4,,                                           '.             
052500   03    FILLER.                                                          
052600     05    FILLER                  PIC X(34)   VALUE                      
052700         '^FO0082,0590^B3N,N,100,N,N^FWN^FDP'.                            
052800     05    ZEBRA-IDARTNR-STRK      PIC X(11).                             
052900     05    FILLER                  PIC X(35)   VALUE                      
053000         '                          '.                                    
053100   03    FILLER.                                                          
053200     05    FILLER                  PIC X(34)   VALUE                      
053300         '^FO0082,0975^B3N,N,050,N,N^FWN^FDQ'.                            
053400     05    ZEBRA-KVINLART-STRK     PIC X(9).                              
053500     05    FILLER                  PIC X(37)   VALUE                      
053600         '                          '.                                    
053700   03    FILLER.                                                          
053800     05    FILLER                  PIC X(34)   VALUE                      
053900         '^FO0082,1100^B3N,N,050,N,N^FWN^FDV'.                            
054000     05    ZEBRA-IDLEVNR-STRK      PIC X(8).                              
054100     05    FILLER                  PIC X(38)   VALUE                      
054200         '                          '.                                    
054300   03    FILLER.                                                          
054400     05    FILLER                  PIC X(34)   VALUE                      
054500         '^FO0082,1250^B3N,N,050,N,N^FWN^FDS'.                            
054600     05    ZEBRA-IDOKOLLI-STRK     PIC X(12).                             
054700     05    FILLER                  PIC X(34)   VALUE                      
054800         '                          '.                                    
054900   03    FILLER                  PIC X(80)   VALUE                        
055000         '^XZ                                              '.             
055100 01  FILLER REDEFINES ZEBRA-TAB.                                          
055200   03  FILLER OCCURS 38.                                                  
055300     05  ZEBRA-RAD              PIC X(80).                                
055400                                                                          
055500     EJECT                                                                
055600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
055700 01  GENERELLA-SUBPROGRAM.                                                
055800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
055900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
056000     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
056100     03  W006PRC1                PIC X(8)    VALUE 'W006PRC1'.            
056200     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
056300     EJECT                                                                
056400*    --- W006PRAR, AREA FÖR W006PRC1 OCH W006PRS1                         
056500 01  FILLER                    PIC X(8)  VALUE 'W006PRAR'.                
056600                                                                          
056700*01  -COPY W006PRAR                                                       
056800     EJECT                                                                
056900 01  FILLER                    PIC X(8)  VALUE 'W006PRVC'.                
057000                                                                          
057100*01  -COPY W006PRVC                                                       
057200     EJECT                                                                
057300*01  -COPY W006PRT                                                        
057400     EJECT                                                                
057500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
057600*                                                                         
057700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
057800     SKIP3                                                                
057900*01  MID -COPY W6I19401                                                   
058000     EJECT                                                                
058100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
058200                                                                          
058300*01  -COPY WMSGAREA                                                       
058400     EJECT                                                                
058500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
058600                                                                          
058700*01  -COPY WMSGSPAR                                                       
058800                                                                          
058900     EJECT                                                                
059000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
059100*                                                                         
059200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
059300                                                                          
059400*    --- STATUS-KOD FRÅN IMS                                              
059500 01  STATUS-WS                   PIC XX.                                  
059600     88  SEGMENT-FINNS                       VALUE '  '.                  
059700                                                                          
059800                                                                          
059900 01  GODK-STATUSKODER.                                                    
060000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
060100                                                                          
060200                                                                          
060300 01  SSA1                        PIC X(64).                               
060400                                                                          
060500     EJECT                                                                
060600*    --- IMS FUNKTIONSKODER                                               
060700*01  -COPY W0003                                                          
060800     EJECT                                                                
060900 LINKAGE SECTION.                                                         
061000                                                                          
061100*01  -COPY W0009   -PRE MSG-                                              
061200     EJECT                                                                
061300*01  -COPY W0009   -PRE ALT-                                              
061400     EJECT                                                                
061500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB.                               
061600 MAIN SECTION.                                                            
061700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB.                               
061800                                                                          
061900     PERFORM IMS-GET-MSG                                                  
062000     IF SEGMENT-FINNS                                                     
062100       PERFORM A-INIT                                                     
062200                                                                          
062300       MOVE MID-IDPRTLST         TO WS-IDPRTLST                           
062400                                    PRT-IDPRTLST                          
062410*      DISPLAY ' IDPRT '  WS-IDPRTLST                                     
062500       MOVE 001                  TO PRT-KDCALL                            
062510                                                                          
062600       CALL W006PRT USING PRT-W006PRT                                     
062601                                                                          
062800       PERFORM S01-PRT-OPEN                                               
062900                                                                          
063000       MOVE +1                   TO IX-POST                               
063100                                                                          
063200       PERFORM UNTIL (IX-POST > MAX-IX-POST                               
063300                  OR  IX-POST > MID-KVPOST)                               
063400                                                                          
063500         PERFORM B-RED-SKR-SIDA                                           
063600         ADD +1                  TO IX-POST                               
063700       END-PERFORM                                                        
063800                                                                          
063900       PERFORM S03-PRT-CLOSE                                              
064000     END-IF                                                               
064100                                                                          
064200     MOVE ZERO TO RETURN-CODE                                             
064300     GOBACK                                                               
064400     .                                                                    
064500                                                                          
064600     EJECT                                                                
064700 A-INIT SECTION.                                                          
064800                                                                          
064900     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I19401                    
065000     MOVE MSG-IDTRANS-1      TO MSG-SPAR-IDTRANS                          
065100     MOVE MSG-KDMFSFOR-1     TO MSG-SPAR-KDMFSFOR                         
065200                                                                          
065300     MOVE MSG-KDTRTYP TO MSG-SPAR-KDTRTYP                                 
065400     MOVE MSG-IDPFK TO MSG-SPAR-IDPFK                                     
065500     MOVE MSG-SPAR-IDTRANS TO W-IDTRANS                                   
065600                                                                          
065700     MOVE LOW-VALUE TO MSG-AREA                                           
065800     .                                                                    
065900                                                                          
066000     EJECT                                                                
066100 B-RED-SKR-SIDA SECTION.                                                  
066200                                                                          
066300     MOVE PRT-NYSIDA-RAD1      TO PRT-RADSKIP                             
066400                                                                          
066500     MOVE SPACE TO LIST1-ARTIKEL-RAD                                      
066600                                                                          
066700     PERFORM S04-RED-STRK                                                 
066800                                                                          
066900     IF FLAGG-TAB-IDARTNR = SPACE                                         
067000       MOVE 'DIVERSE '           TO NOVA-TAB-IDARTNR                      
067100       MOVE 'DIVERSE '           TO FLAGG-TAB-IDARTNR                     
067200                                    FLAGG-TAB-IDARTNR-VCOM                
067300       MOVE 'MMIXED  '           TO ZEBRA-IDARTNR                         
067400     ELSE                                                                 
067410       MOVE MID-IDARTNR (IX-POST) TO NOVA-TAB-IDARTNRZ                    
067420                                                                          
067610       INSPECT NOVA-TAB-KVINLART  REPLACING LEADING ZEROS                 
067620                               BY SPACE                                   
067700       INSPECT FLAGG-TAB-IDARTNR REPLACING LEADING ZEROS                  
067800                               BY SPACE                                   
067900       INSPECT FLAGG-TAB-IDARTNR-VCOM REPLACING LEADING ZEROS             
068000                               BY SPACE                                   
068100       INSPECT ZEBRA-IDARTNR     REPLACING LEADING ZEROS                  
068200                               BY SPACE                                   
068300     END-IF                                                               
068400                                                                          
068500     IF PRT-BEPRTLST(1:4) = 'NOVA'                                        
068501       MOVE MID-IDOKOLLI (IX-POST) TO NOVA-TAB-IDOKOLLI                   
068502     END-IF                                                               
068510                                                                          
068520     MOVE MID-KVINLART (IX-POST) TO ZEBRA-KVINLART                        
068600     MOVE MID-IDLEVNR-KOLLI  (IX-POST) TO ZEBRA-IDLEVNR                   
068700     MOVE MID-IDOKOLLI (IX-POST) TO ZEBRA-IDOKOLLI                        
068800                                                                          
068900     MOVE MID-IDLOPNRM (IX-POST) TO NOVA-TAB-IDLOPNRM                     
069000     MOVE MID-IDLOPNRM (IX-POST) TO FLAGG-TAB-IDLOPNRM                    
069100                                    FLAGG-TAB-IDLOPNRM-VCOM               
069200                                    ZEBRA-IDLOPNRM                        
069300     MOVE MID-ADLAGOMR (IX-POST) TO NOVA-TAB-ADLAGOMR                     
069400     MOVE MID-ADLAGOMR (IX-POST) TO FLAGG-TAB-ADLAGOMR                    
069500                                    FLAGG-TAB-ADLAGOMR-VCOM               
069600                                    ZEBRA-ADLAGOMR                        
069700     MOVE MID-ADGANG   (IX-POST) TO NOVA-TAB-ADGANG                       
069800     MOVE MID-ADGANG   (IX-POST) TO FLAGG-TAB-ADGANG                      
069900                                    FLAGG-TAB-ADGANG-VCOM                 
070000                                    ZEBRA-ADGANG                          
070100     MOVE MID-ADPLATS  (IX-POST) TO NOVA-TAB-ADPLATS                      
070200     MOVE MID-ADPLATS  (IX-POST) TO FLAGG-TAB-ADPLATS                     
070300                                    FLAGG-TAB-ADPLATS-VCOM                
070400                                    ZEBRA-ADPLATS                         
070500     MOVE MID-KDSORT   (IX-POST) TO NOVA-TAB-KDSORT                       
070600     MOVE MID-KDSORT   (IX-POST) TO FLAGG-TAB-KDSORT                      
070700                                    FLAGG-TAB-KDSORT-VCOM                 
070800     MOVE MID-VKKOLLIN (IX-POST) TO NOVA-TAB-VKARTNTO                     
070900     MOVE MID-VKKOLLIN (IX-POST) TO FLAGG-TAB-VKARTNTO                    
071000                                    FLAGG-TAB-VKARTNTO-VCOM               
071100                                    ZEBRA-VKARTNTO                        
071200     MOVE MID-TIINLMOT (IX-POST) TO NOVA-TAB-TIAVIDAT                     
071300     MOVE MID-TIINLMOT (IX-POST) TO FLAGG-TAB-TIAVIDAT                    
071400                                    FLAGG-TAB-TIAVIDAT-VCOM               
071500                                    ZEBRA-TIAVIDAT                        
071600                                                                          
071700     IF PRT-BEPRTLST(1:5) = 'ZEBRA'                                       
071800       MOVE +1 TO ZEBRA-IX                                                
071900       PERFORM UNTIL ZEBRA-IX > MAX-IX-ZEBRA                              
072000         MOVE ZEBRA-RAD (ZEBRA-IX) TO LIST1-ARTIKEL-RAD                   
072100         CALL W006PRS1 USING         PRT-SPOOL-A4S                        
072200                                     PRT-WRITE                            
072300                                     WS-IDPRTLST                          
072400                                     ALT-PCB                              
072500                                     PRT-RADSKIP                          
072600                                     LIST1-ARTIKEL-RAD                    
072700         MOVE PRT-AFTER-1          TO PRT-RADSKIP                         
072800         ADD +1 TO ZEBRA-IX                                               
072900       END-PERFORM                                                        
073000     ELSE                                                                 
073100       IF PRT-BEPRTLST(1:4) = 'VCOM'                                      
073110         CONTINUE                                                         
073200*        MOVE +1 TO FLAGG-IX-VCOM                                         
073300*        PERFORM UNTIL FLAGG-IX-VCOM > MAX-IX-VCOM                        
073400*          MOVE FLAGG-RAD-VCOM (FLAGG-IX-VCOM) TO PRC1-DATA               
073500*          CALL W006PRC1 USING         PRT-VCOM                           
073600*                                      PRT-WRITE                          
073700*                                      WS-IDPRTLST                        
073800*                                      ALT-PCB                            
073900*                                      PRC1-W006PRVC                      
074000*          MOVE PRT-AFTER-1          TO PRT-RADSKIP                       
074100*          ADD +1 TO FLAGG-IX-VCOM                                        
074200*        END-PERFORM                                                      
074300       ELSE                                                               
074400         IF PRT-BEPRTLST(1:4) = 'NOVA'                                    
074500           MOVE +1 TO FLAGG-IX-NOVA                                       
074600           PERFORM UNTIL FLAGG-IX-NOVA > MAX-IX-NOVA                      
074700             MOVE FLAGG-RAD-NOVA (FLAGG-IX-NOVA)                          
074800               TO LIST1-ARTIKEL-RAD                                       
074900             CALL W006PRS1 USING       PRT-SPOOL-A4S                      
075000                                       PRT-WRITE                          
075100                                       WS-IDPRTLST                        
075200                                       ALT-PCB                            
075500                                       PRT-AFTER-1                        
075510                                       LIST1-ARTIKEL-RAD                  
075600             ADD +1 TO FLAGG-IX-NOVA                                      
075700           END-PERFORM                                                    
075800         ELSE                                                             
075900           MOVE +1 TO FLAGG-IX                                            
076000           PERFORM UNTIL FLAGG-IX > MAX-IX                                
076100             MOVE FLAGG-RAD (FLAGG-IX) TO LIST1-ARTIKEL-RAD               
076200             CALL W006PRS1 USING       PRT-SPOOL-A4S                      
076300                                         PRT-WRITE                        
076400                                         WS-IDPRTLST                      
076500                                         ALT-PCB                          
076510                                         PRT-RADSKIP                      
076600                                         LIST1-ARTIKEL-RAD                
076700             MOVE PRT-AFTER-1        TO PRT-RADSKIP                       
076800             ADD +1 TO FLAGG-IX                                           
076900           END-PERFORM                                                    
077000         END-IF                                                           
077100       END-IF                                                             
077200     END-IF                                                               
077300                                                                          
077400     .                                                                    
077500                                                                          
077600     EJECT                                                                
077700 S01-PRT-OPEN  SECTION.                                                   
077800                                                                          
077900*    MOVE 'W601Z1SE'       TO    PRT-IDVCOM                               
078000     MOVE 'W601FLAA'       TO    PRT-IDCPYTXT                             
078100     MOVE +120             TO    PRC1-KVLRECL                             
078200     MOVE 'W006ASCI'       TO    PRC1-IDVCINIT                            
078300     MOVE 'W006PRT '       TO    PRC1-TEVCOMST                            
078400     MOVE SPACE            TO    PRC1-DATA                                
078500                                                                          
078600     IF PRT-BEPRTLST(1:4) = 'VCOM'                                        
078610       CONTINUE                                                           
078700*      CALL W006PRC1 USING         PRT-VCOM                               
078800*                                  PRT-OPEN                               
078900*                                  WS-IDPRTLST                            
079000*                                  ALT-PCB                                
079100*                                  PRC1-W006PRVC                          
079200     ELSE                                                                 
079300       CALL W006PRS1 USING         PRT-SPOOL-A4S                          
079400                                   PRT-OPEN                               
079500                                   WS-IDPRTLST                            
079600                                   ALT-PCB                                
079700                                   PRT-FILLER                             
079800                                   PRT-FILLER                             
079900     END-IF                                                               
080000     .                                                                    
080100                                                                          
080200     EJECT                                                                
080300 S03-PRT-CLOSE SECTION.                                                   
080400                                                                          
080500     IF PRT-BEPRTLST(1:4) = 'VCOM'                                        
080510       CONTINUE                                                           
080600*      CALL W006PRC1 USING         PRT-VCOM                               
080700*                                  PRT-CLOSE                              
080800*                                  WS-IDPRTLST                            
080900*                                  ALT-PCB                                
081000*                                  PRC1-W006PRVC                          
081100     ELSE                                                                 
081200       CALL W006PRS1 USING         PRT-SPOOL-A4S                          
081300                                   PRT-CLOSE                              
081400                                   WS-IDPRTLST                            
081500                                   ALT-PCB                                
081600                                   PRT-FILLER                             
081700                                   PRT-FILLER                             
081800     END-IF                                                               
081900     .                                                                    
082000                                                                          
082100     EJECT                                                                
082200 S04-RED-STRK SECTION.                                                    
082300                                                                          
082400*    FIXA ANVÄNDS FÖR ATT TA BORT INLEDANDE NOLLOR                        
082500*    GENOM ATT FLYTTA ETT STEG TILL VÄNSTER                               
082600                                                                          
082700*REDIGERING AV IDARTNR  STRECKKODEN.                                      
082800                                                                          
082900     MOVE MID-IDARTNR (IX-POST) TO ZEBRA-IDARTNR                          
083000     MOVE SPACE TO FIXA                                                   
083100     STRING MID-IDARTNR (IX-POST) '^FS'                                   
083200         DELIMITED BY SIZE INTO FIXA                                      
083300** fix för att få rätt positionering av artikelnummer                     
083500     IF MID-IDPRTLST NOT = '6FBIMA  '                                     
083700     PERFORM UNTIL FIX1 NOT = '0'                                         
083800       MOVE FIXB TO FIXA                                                  
083900     END-PERFORM                                                          
084100     END-IF                                                               
084300     MOVE FIXA TO ZEBRA-IDARTNR-STRK                                      
084400     INSPECT FIXA REPLACING ALL '^FS' BY '"  '                            
084500     MOVE FIXA TO FLAGG-TAB-IDARTNR-STRK                                  
084600     INSPECT FIXA REPLACING ALL '"  ' BY SPACE                            
084700     MOVE FIXA TO NOVA-TAB-IDARTNR-STRK                                   
084800     INSPECT FIXA REPLACING ALL '"  ' BY SPACE                            
084900     MOVE FIXA TO FLAGG-TAB-IDARTNR                                       
085000     MOVE FIXA TO FLAGG-TAB-IDARTNR-VCOM                                  
085100                                                                          
085200                                                                          
085300*REDIGERING AV KVINLART STRECKKODEN.                                      
085400                                                                          
085500     MOVE SPACE TO FIXA                                                   
085600     STRING MID-KVINLART (IX-POST) '^FS'                                  
085700         DELIMITED BY SIZE INTO FIXA                                      
085800     PERFORM UNTIL FIX1 NOT = '0'                                         
085900       MOVE FIXB TO FIXA                                                  
086000     END-PERFORM                                                          
086100     MOVE FIXA TO ZEBRA-KVINLART-STRK                                     
086200     INSPECT FIXA REPLACING ALL '^FS' BY '"  '                            
086300     MOVE FIXA TO FLAGG-TAB-KVINLART-STRK                                 
086400     INSPECT FIXA REPLACING ALL '"  ' BY SPACE                            
086500     MOVE FIXA TO NOVA-TAB-KVINLART-STRK                                  
086600     INSPECT FIXA REPLACING ALL '"  ' BY SPACE                            
086710     MOVE FIXA TO FLAGG-TAB-KVINLART                                      
086800     MOVE FIXA TO FLAGG-TAB-KVINLART-VCOM                                 
086810     MOVE FIXA TO NOVA-TAB-KVINLART                                       
086900                                                                          
087000                                                                          
087100*REDIGERING AV IDLEVNR-KOLLI STRECKKODEN.                                 
087200                                                                          
087300     MOVE SPACE TO FIXA                                                   
087400     STRING MID-IDLEVNR-KOLLI (IX-POST) '^FS'                             
087500         DELIMITED BY SIZE INTO FIXA                                      
087900     MOVE FIXA TO ZEBRA-IDLEVNR-STRK                                      
088000     INSPECT FIXA REPLACING ALL '^FS' BY '"  '                            
088100     MOVE FIXA TO FLAGG-TAB-IDLEVNR-STRK                                  
088200     INSPECT FIXA REPLACING ALL '"  ' BY SPACE                            
088300     MOVE FIXA TO NOVA-TAB-IDLEVNR-STRK                                   
088400     INSPECT FIXA REPLACING ALL '"  ' BY SPACE                            
088500     MOVE FIXA TO NOVA-TAB-IDLEVNR                                        
088510     MOVE FIXA TO FLAGG-TAB-IDLEVNR                                       
088600     MOVE FIXA TO FLAGG-TAB-IDLEVNR-VCOM                                  
088700                                                                          
088800                                                                          
088900*REDIGERING AV IDOKOLLI STRECKKODEN.                                      
089000                                                                          
089100     MOVE SPACE TO FIXA                                                   
089200     STRING MID-IDOKOLLI (IX-POST) '^FS'                                  
089300         DELIMITED BY SIZE INTO FIXA                                      
089400     PERFORM UNTIL FIX1 NOT = '0'                                         
089500       MOVE FIXB TO FIXA                                                  
089600     END-PERFORM                                                          
089700     MOVE FIXA TO ZEBRA-IDOKOLLI-STRK                                     
089800     INSPECT FIXA REPLACING ALL '^FS' BY '"  '                            
089900     MOVE FIXA TO FLAGG-TAB-IDOKOLLI-STRK                                 
090000     INSPECT FIXA REPLACING ALL '"  ' BY SPACE                            
090100     MOVE FIXA TO NOVA-TAB-IDOKOLLI-STRK                                  
090200     INSPECT FIXA REPLACING ALL '"  ' BY SPACE                            
090310     MOVE FIXA TO FLAGG-TAB-IDOKOLLI                                      
090400     MOVE FIXA TO FLAGG-TAB-IDOKOLLI-VCOM                                 
090500     .                                                                    
090600                                                                          
090700     EJECT                                                                
090800* --- IMS SEKTIONER ---                                                   
090900                                                                          
091000                                                                          
091100 IMS-GET-MSG SECTION.                                                     
091200                                                                          
091300     MOVE '  QC' TO GODK-STATUSKODER                                      
091400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
091500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
091600     PERFORM IMS-STATUSKONTROLL                                           
091700     .                                                                    
091800                                                                          
091900                                                                          
092000                                                                          
092100 IMS-STATUSKONTROLL SECTION.                                              
092200                                                                          
092300     SET STATUS-IX TO 1                                                   
092400     SEARCH GODK-STATUS                                                   
092500       AT END                                                             
092600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
092700         DELIMITED BY SIZE INTO FELTEXT                                   
092800         CALL FELLOG                                                      
092900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
093000         CONTINUE                                                         
093100     END-SEARCH                                                           
093200     .                                                                    
