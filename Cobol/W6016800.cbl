000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6016800.                                                
000300 AUTHOR.         BODIL LINDAHL / JOHAN LINDKVIST.                         
000400 DATE-WRITTEN.   98/05/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PRINTNING BARCODE ETIKETTER                                      
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001100*                              WLBENA (WDD3)                              
001200*                              WLETIA (WDK3)                              
001300*                              WLINLE (WDL2)                              
001400*                   STARTAR RUTIN XXX                                     
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W6T168                                              
001800*        MID:         W6I16801                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W6O16801                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*                                                                         
003000 77  IDPGM                       PIC X(08)   VALUE 'W6016800'.            
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200 77  KDRC-DISPLAY                PIC Z(5).                                
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500*    -- CHECKED BY WY2000                                                 
003600*                                                                         
003700*                                                                         
003800* VID TILLÄGG AV NYA SKRIVARE/PRINTERS.   OBSERVERA FÖLJANDE:             
003900*             1) LÄGG TILL FILLER X(8)  MED 'SHORTNAME'                   
004000*             2) LÄGG TILL FILLER X(3) MED 'PRINTER TYP'                  
004100*             3) LÄGG TILL FILLER X(40) MED 'NT PRINTERADRESS'            
004200*             4) ÖKA 'GODK-PRINTER OCCURS' MED ETT STEG                   
004300*             5) KOMPILERA OM, SEDAN ÄR DET KLART !                       
004400*                                                                         
004500 01  TABELL.                                                              
004600     03  PRINTERDEFINITIONS.                                              
004700*    SKRIVARE 01                                                          
004800         05 FILLER               PIC X(8) VALUE '584-1X'.                 
004900         05 FILLER               PIC X(3) VALUE '8XD'.                    
005000         05 FILLER               PIC X(40) VALUE                          
005100                       '584-1;LPT1:                            '.         
005200*    SKRIVARE 02                                                          
005300         05 FILLER               PIC X(8) VALUE '584-2X'.                 
005400         05 FILLER               PIC X(3) VALUE '5XD'.                    
005500         05 FILLER               PIC X(40) VALUE                          
005600                       '584-2;LPT1:                            '.         
005700*    SKRIVARE 03                                                          
005800         05 FILLER               PIC X(8) VALUE '584-3X'.                 
005900         05 FILLER               PIC X(3) VALUE '8XD'.                    
006000         05 FILLER               PIC X(40) VALUE                          
006100                       '584-3;LPT1:                            '.         
006200*    SKRIVARE 04                                                          
006300         05 FILLER               PIC X(8) VALUE '584-1'.                  
006400         05 FILLER               PIC X(3) VALUE '872'.                    
006500         05 FILLER               PIC X(40) VALUE                          
006600                       '584-1;LPT1:                            '.         
006700*    SKRIVARE 05                                                          
006800         05 FILLER               PIC X(8) VALUE '584-2'.                  
006900         05 FILLER               PIC X(3) VALUE '572'.                    
007000         05 FILLER               PIC X(40) VALUE                          
007100                       '584-2;LPT1:                            '.         
007200*    SKRIVARE 06                                                          
007300         05 FILLER               PIC X(8) VALUE '584-3'.                  
007400         05 FILLER               PIC X(3) VALUE '872'.                    
007500         05 FILLER               PIC X(40) VALUE                          
007600                       '584-3;LPT1:                            '.         
007700*    SKRIVARE 07                                                          
007800         05 FILLER               PIC X(8) VALUE '573-1'.                  
007900         05 FILLER               PIC X(3) VALUE '572'.                    
008000         05 FILLER               PIC X(40) VALUE                          
008100                       '573-1;LPT1:                            '.         
008200*    SKRIVARE 08                                                          
008300         05 FILLER               PIC X(8) VALUE '573-3'.                  
008400         05 FILLER               PIC X(3) VALUE '572'.                    
008500         05 FILLER               PIC X(40) VALUE                          
008600                       '573-3;LPT1:                            '.         
008700*    SKRIVARE 09                                                          
008800         05 FILLER               PIC X(8) VALUE '573-2'.                  
008900         05 FILLER               PIC X(3) VALUE '872'.                    
009000         05 FILLER               PIC X(40) VALUE                          
009100                       '573-2;LPT1:                            '.         
009200*    SKRIVARE 10                                                          
009300         05 FILLER               PIC X(8) VALUE '413-5'.                  
009400         05 FILLER               PIC X(3) VALUE '572'.                    
009500         05 FILLER               PIC X(40) VALUE                          
009600                       '413-5;LPT1:                            '.         
009700*    SKRIVARE 11                                                          
009800         05 FILLER               PIC X(8) VALUE '413-6'.                  
009900         05 FILLER               PIC X(3) VALUE '872'.                    
010000         05 FILLER               PIC X(40) VALUE                          
010100                       '413-6;LPT1:                            '.         
010200*    SKRIVARE 12                                                          
010300         05 FILLER               PIC X(8) VALUE '565-3'.                  
010400         05 FILLER               PIC X(3) VALUE '572'.                    
010500         05 FILLER               PIC X(40) VALUE                          
010600                       '565-3;LPT1:                            '.         
010700*    SKRIVARE 13          (NOVECC CABRIO SKRIVARE)                        
010800         05 FILLER               PIC X(8) VALUE '413-7'.                  
010900         05 FILLER               PIC X(3) VALUE '572'.                    
011000         05 FILLER               PIC X(40) VALUE                          
011100                       '413-7;LPT1:                            '.         
011200*    SKRIVARE 14                                                          
011300         05 FILLER               PIC X(8) VALUE '583-2'.                  
011400         05 FILLER               PIC X(3) VALUE '872'.                    
011500         05 FILLER               PIC X(40) VALUE                          
011600                       '583-2;LPT1:                            '.         
011700*    SKRIVARE 15                                                          
011800         05 FILLER               PIC X(8) VALUE '569-1'.                  
011900         05 FILLER               PIC X(3) VALUE '872'.                    
012000         05 FILLER               PIC X(40) VALUE                          
012100                       '569-1;LPT1:                            '.         
012200*    SKRIVARE 16                                                          
012300         05 FILLER               PIC X(8) VALUE '565-1'.                  
012400         05 FILLER               PIC X(3) VALUE '572'.                    
012500         05 FILLER               PIC X(40) VALUE                          
012600                       '565-1;LPT1:                            '.         
012700*    SKRIVARE 17                                                          
012800         05 FILLER               PIC X(8) VALUE '565-2'.                  
012900         05 FILLER               PIC X(3) VALUE '872'.                    
013000         05 FILLER               PIC X(40) VALUE                          
013100                       '565-2;LPT1:                            '.         
013200*    SKRIVARE 18                                                          
013300         05 FILLER               PIC X(8) VALUE '413-1'.                  
013400         05 FILLER               PIC X(3) VALUE '572'.                    
013500         05 FILLER               PIC X(40) VALUE                          
013600                       '413-1;LPT1:                            '.         
013700*    SKRIVARE 19                                                          
013800         05 FILLER               PIC X(8) VALUE '413-2'.                  
013900         05 FILLER               PIC X(3) VALUE '872'.                    
014000         05 FILLER               PIC X(40) VALUE                          
014100                       '413-2;LPT1:                            '.         
014200*    SKRIVARE 20                                                          
014300         05 FILLER               PIC X(8) VALUE '413-3'.                  
014400         05 FILLER               PIC X(3) VALUE '872'.                    
014500         05 FILLER               PIC X(40) VALUE                          
014600                       '413-3;LPT1:                            '.         
014700*    SKRIVARE 21                                                          
014800         05 FILLER               PIC X(8) VALUE '414-4'.                  
014900         05 FILLER               PIC X(3) VALUE '572'.                    
015000         05 FILLER               PIC X(40) VALUE                          
015100                       '414-4;LPT1:                            '.         
015200*    SKRIVARE 22                                                          
015300         05 FILLER               PIC X(8) VALUE '584-4'.                  
015400         05 FILLER               PIC X(3) VALUE '572'.                    
015500         05 FILLER               PIC X(40) VALUE                          
015600                       '584-4;LPT1:                            '.         
015700*    SKRIVARE 23                                                          
015800         05 FILLER               PIC X(8) VALUE '584-4X'.                 
015900         05 FILLER               PIC X(3) VALUE '5XD'.                    
016000         05 FILLER               PIC X(40) VALUE                          
016100                       '584-4;LPT1:                            '.         
016200*    SKRIVARE 24                                                          
016300         05 FILLER               PIC X(8) VALUE '666-1'.                  
016400         05 FILLER               PIC X(3) VALUE '572'.                    
016500         05 FILLER               PIC X(40) VALUE                          
016600                       '666-1;LPT1:                            '.         
016700*    SKRIVARE 25                                                          
016800         05 FILLER               PIC X(8) VALUE '666-2'.                  
016900         05 FILLER               PIC X(3) VALUE '872'.                    
017000         05 FILLER               PIC X(40) VALUE                          
017100                       '666-2;LPT1:                            '.         
017200                                                                          
017300*    SKRIVARE 26                                                          
017400         05 FILLER               PIC X(8) VALUE '413-4'.                  
017500         05 FILLER               PIC X(3) VALUE '872'.                    
017600         05 FILLER               PIC X(40) VALUE                          
017700                       '413-4;LPT1:                            '.         
017800                                                                          
017900*    SKRIVARE 27                                                          
018000         05 FILLER               PIC X(8) VALUE '413-8'.                  
018100         05 FILLER               PIC X(3) VALUE '872'.                    
018200         05 FILLER               PIC X(40) VALUE                          
018300                       '413-8;LPT1:                            '.         
018400                                                                          
018500*    SKRIVARE 28                                                          
018600         05 FILLER               PIC X(8) VALUE '565-4'.                  
018700         05 FILLER               PIC X(3) VALUE '572'.                    
018800         05 FILLER               PIC X(40) VALUE                          
018900                       '565-4;LPT1:                            '.         
019000                                                                          
019100*    SKRIVARE 29                                                          
019200         05 FILLER               PIC X(8) VALUE '514-1'.                  
019300         05 FILLER               PIC X(3) VALUE '872'.                    
019400         05 FILLER               PIC X(40) VALUE                          
019500                       '514-1;LPT1:                            '.         
019600                                                                          
019700*    SKRIVARE 30                                                          
019800         05 FILLER               PIC X(8) VALUE '163-1'.                  
019900         05 FILLER               PIC X(3) VALUE '872'.                    
020000         05 FILLER               PIC X(40) VALUE                          
020100                       '163-1;LPT1:                            '.         
020200                                                                          
020300*    SKRIVARE 31                                                          
020400         05 FILLER               PIC X(8) VALUE '420-1'.                  
020500         05 FILLER               PIC X(3) VALUE '872'.                    
020600         05 FILLER               PIC X(40) VALUE                          
020700                       '420-1;LPT1:                            '.         
020800                                                                          
020900*    SKRIVARE 32                                                          
021000         05 FILLER               PIC X(8) VALUE '441-1'.                  
021100         05 FILLER               PIC X(3) VALUE '572'.                    
021200         05 FILLER               PIC X(40) VALUE                          
021300                       '441-1;LPT1:                            '.         
021400                                                                          
021500*    SKRIVARE 33                                                          
021600         05 FILLER               PIC X(8) VALUE '523-1'.                  
021700         05 FILLER               PIC X(3) VALUE '572'.                    
021800         05 FILLER               PIC X(40) VALUE                          
021900                       '523-1;LPT1:                            '.         
022000                                                                          
022100*    SKRIVARE 34                                                          
022200         05 FILLER               PIC X(8) VALUE '414-2'.                  
022300         05 FILLER               PIC X(3) VALUE '572'.                    
022400         05 FILLER               PIC X(40) VALUE                          
022500                       '414-2;LPT1:                            '.         
022600                                                                          
022700*    SKRIVARE 35                                                          
022800         05 FILLER               PIC X(8) VALUE '513-1'.                  
022900         05 FILLER               PIC X(3) VALUE '572'.                    
023000         05 FILLER               PIC X(40) VALUE                          
023100                       '513-1;LPT1:                            '.         
023200                                                                          
023300*    SKRIVARE 36                                                          
023400         05 FILLER               PIC X(8) VALUE '163-2'.                  
023500         05 FILLER               PIC X(3) VALUE '872'.                    
023600         05 FILLER               PIC X(40) VALUE                          
023700                       '163-2;LPT1:                            '.         
023800                                                                          
023900*                                                                         
024000*                                                                         
024100*   OBS OBS !!!    GLÖM EJ ATT ÖKA TABELL NEDAN    !!! OBS OBS            
024200*   OBS OBS !!!    GLÖM EJ ATT ÖKA TABELL NEDAN    !!! OBS OBS            
024300*   OBS OBS !!!    GLÖM EJ ATT ÖKA TABELL NEDAN    !!! OBS OBS            
024400*                                                                         
024500     03  GODK-PRINTERS REDEFINES PRINTERDEFINITIONS.                      
024600         05 GODK-PRINTER OCCURS 36 INDEXED BY PRINTER-IX.                 
024700            07 WS-IDPRTLST       PIC X(8).                                
024800            07 WS-PRINTERTYP     PIC X(3).                                
024900            07 WS-IDPCPRT        PIC X(40).                               
025000                                                                          
025100*                                                                         
025200****************************                                              
025300*                                                                         
025400* DATA TO BE SENT TO MQ.                                                  
025500*                                                                         
025600                                                                          
025700 01 MQ-DATA-TABELL.                                                       
025800     03  MQ-LOAD.                                                         
025900      05 FILLER         PIC X(23) VALUE 'LOAD(D:\BARCODE\LABELS\'.        
026000      05 MQ-LAYOUT          PIC X(15).                                    
026100      05 FILLER             PIC X(45) VALUE SPACE.                        
026200*                                                                         
026300     03  MQ-SETPRINTER.                                                   
026400      05 FILLER             PIC X(11) VALUE 'SETPRINTER('.                
026500      05 MQ-IDPCPRT-JL1     PIC X(41).                                    
026600      05 FILLER             PIC X(31) VALUE SPACE.                        
026700*                                                                         
026800     03  MQ-INITPRINTER.                                                  
026900      05 FILLER             PIC X(11) VALUE 'INITPRINTER'.                
027000      05 FILLER             PIC X(72) VALUE SPACE.                        
027100*                                                                         
027200***   ***   ***   ***   ***                                               
027300*                                                                         
027400     03  MQ-SETDATA-IDARTNR.                                              
027500      05 FILLER             PIC X(16) VALUE 'SETDATA(IDARTNR;'.           
027600      05 MQ-IDARTNR         PIC X(10).                                    
027700      05 FILLER             PIC X(57) VALUE SPACE.                        
027800*                                                                         
027900     03  MQ-SETDATA-IDSORTIM.                                             
028000      05 FILLER             PIC X(15) VALUE 'SETDATA(SORTIM;'.            
028100      05 MQ-IDSORTIM        PIC X(03) VALUE 'VO '.                        
028200      05 FILLER             PIC X(1)  VALUE ')'.                          
028300      05 FILLER             PIC X(64) VALUE SPACE.                        
028400*                                                                         
028500     03  MQ-SETDATA-BEARTURS.                                             
028600      05 FILLER             PIC X(17) VALUE 'SETDATA(BEARTURS;'.          
028700      05 MQ-BEARTURS        PIC X(15).                                    
028800      05 FILLER             PIC X(1)  VALUE ')'.                          
028900      05 FILLER             PIC X(50) VALUE SPACE.                        
029000*                                                                         
029100     03  MQ-SETDATA-KVQPACK.                                              
029200      05 FILLER             PIC X(16) VALUE 'SETDATA(KVQPACK;'.           
029300      05 MQ-KVQPACK         PIC X(13).                                    
029400      05 FILLER             PIC X(1)  VALUE ')'.                          
029500      05 FILLER             PIC X(53) VALUE SPACE.                        
029600*                                                                         
029700     03  MQ-SETDATA-IDBATCH.                                              
029800      05 FILLER             PIC X(16) VALUE 'SETDATA(IDBATCH;'.           
029900      05 MQ-IDBATCH         PIC X(18).                                    
030000      05 FILLER             PIC X(1)  VALUE ')'.                          
030100      05 FILLER             PIC X(48) VALUE SPACE.                        
030200*                                                                         
030300     03  MQ-SETDATA-DATUMKOD.                                             
030400      05 FILLER             PIC X(17) VALUE 'SETDATA(DATUMKOD;'.          
030500      05 MQ-DATUMKOD        PIC X(02).                                    
030600      05 FILLER             PIC X(1)  VALUE ')'.                          
030700      05 FILLER             PIC X(63) VALUE SPACE.                        
030800*                                                                         
030900     03  MQ-SETDATA-TEETIK-EXT-01.                                        
031000      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-01;'.         
031100      05 MQ-TEETIK-EXT-01 PIC X(60) VALUE SPACE.                          
031200      05 FILLER             PIC X(1)  VALUE ')'.                          
031300*                                                                         
031400     03  MQ-SETDATA-TEETIK-EXT-02.                                        
031500      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-02;'.         
031600      05 MQ-TEETIK-EXT-02 PIC X(60) VALUE SPACE.                          
031700      05 FILLER             PIC X(1)  VALUE ')'.                          
031800*                                                                         
031900     03  MQ-SETDATA-TEETIK-EXT-03.                                        
032000      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-03;'.         
032100      05 MQ-TEETIK-EXT-03 PIC X(60) VALUE SPACE.                          
032200      05 FILLER             PIC X(1)  VALUE ')'.                          
032300*                                                                         
032400     03  MQ-SETDATA-TEETIK-EXT-04.                                        
032500      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-04;'.         
032600      05 MQ-TEETIK-EXT-04 PIC X(60) VALUE SPACE.                          
032700      05 FILLER             PIC X(1)  VALUE ')'.                          
032800*                                                                         
032900     03  MQ-SETDATA-TEETIK-EXT-05.                                        
033000      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-05;'.         
033100      05 MQ-TEETIK-EXT-05 PIC X(60) VALUE SPACE.                          
033200      05 FILLER             PIC X(1)  VALUE ')'.                          
033300*                                                                         
033400     03  MQ-SETDATA-TEETIK-EXT-06.                                        
033500      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-06;'.         
033600      05 MQ-TEETIK-EXT-06 PIC X(60) VALUE SPACE.                          
033700      05 FILLER             PIC X(1)  VALUE ')'.                          
033800*                                                                         
033900     03  MQ-SETDATA-TEETIK-EXT-07.                                        
034000      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-07;'.         
034100      05 MQ-TEETIK-EXT-07 PIC X(60) VALUE SPACE.                          
034200      05 FILLER             PIC X(1)  VALUE ')'.                          
034300*                                                                         
034400     03  MQ-SETDATA-TEETIK-EXT-08.                                        
034500      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-08;'.         
034600      05 MQ-TEETIK-EXT-08 PIC X(60) VALUE SPACE.                          
034700      05 FILLER             PIC X(1)  VALUE ')'.                          
034800*                                                                         
034900     03  MQ-SETDATA-TEETIK-EXT-09.                                        
035000      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-09;'.         
035100      05 MQ-TEETIK-EXT-09 PIC X(60) VALUE SPACE.                          
035200      05 FILLER             PIC X(1)  VALUE ')'.                          
035300*                                                                         
035400     03  MQ-SETDATA-TEETIK-EXT-10.                                        
035500      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-10;'.         
035600      05 MQ-TEETIK-EXT-10 PIC X(60) VALUE SPACE.                          
035700      05 FILLER             PIC X(1)  VALUE ')'.                          
035800*                                                                         
035900     03  MQ-SETDATA-TEETIK-EXT-11.                                        
036000      05 FILLER         PIC X(22) VALUE 'SETDATA(TEETIK-EXT-11;'.         
036100      05 MQ-TEETIK-EXT-11 PIC X(60) VALUE SPACE.                          
036200      05 FILLER             PIC X(1)  VALUE ')'.                          
036300* FLYTTAT !!!                                                             
036400* FLYTTAT !!!                                                             
036500     03  MQ-SETSHOWFIELD.                                                 
036600      05 FILLER             PIC X(22)                                     
036700                            VALUE 'SETSHOWFIELD(DATUMKOD;'.               
036800      05 MQ-SHOW            PIC 9.                                        
036900      05 FILLER             PIC X(1)  VALUE ')'.                          
037000      05 FILLER             PIC X(59) VALUE SPACE.                        
037100*                                                                         
037200     03  MQ-PRINT.                                                        
037300      05 FILLER             PIC X(6)  VALUE 'PRINT('.                     
037400      05 MQ-KVANTAL         PIC 9(5).                                     
037500      05 FILLER             PIC X(1)  VALUE ')'.                          
037600      05 FILLER             PIC X(71) VALUE SPACE.                        
037700*                                                                         
037800     03  MQ-CLOSE.                                                        
037900      05 FILLER             PIC X(7)  VALUE 'CLOSE()'.                    
038000      05 FILLER             PIC X(76) VALUE SPACE.                        
038100*                                                                         
038200****************************                                              
038300     03  MQ-SETDATA-TEETIK-EXT-12.                                        
038400      05 FILLER             PIC X(22) VALUE SPACE.                        
038500      05 MQ-TEETIK-EXT-12 PIC X(60) VALUE SPACE.                          
038600      05 FILLER             PIC X(1)  VALUE ' '.                          
038700*                                                                         
038800***   ***   ***   ***   ***                                               
038900 01  FILLER REDEFINES MQ-DATA-TABELL.                                     
039000     03  FILLER    OCCURS 24 TIMES INDEXED BY MQ-IX.                      
039100      05 MQ-AREA            PIC X(83).                                    
039200                                                                          
039300 01  MAX-MQ-IX              PIC S9(4) COMP VALUE +24.                     
039400*                                                                         
039500*                                                                         
039600****************************                                              
039700****  WORKING STORAGE  *****                                              
039800****************************                                              
039900*                                                                         
040000 01  WS-SHOWKOD.                                                          
040100     03 WS-SKRIVARE              PIC X.                                   
040200     03 FILLER                   PIC X(2).                                
040300*                                                                         
040400 01 WS-IDARTNR-GRP.                                                       
040500    03 WS-IDARTNR1         PIC Z(09).                                     
040600*                                                                         
040700 01  WS-MQ-FIELDS.                                                        
040800     03 MQ-IDLAYOUT-PRE.                                                  
040900        05 WS-STORLEK-KOD  PIC X(1)  VALUE SPACES.                        
041000        05 WS-TYP-KOD      PIC X(4)  VALUE SPACES.                        
041100        05 WS-LOEPNUMMER   PIC X(2)  VALUE SPACES.                        
041200        05 WS-SKRIVARE-KOD PIC X     VALUE SPACES.                        
041300        05 WS-LOGO-KOD     PIC X     VALUE SPACES.                        
041400        05 WS-TOMRUM       PIC X(1)  VALUE SPACES.                        
041500     03 MQ-LFE-SUFF        PIC X(4)  VALUE '.LFE'.                        
041600     03 MQ-IDLAYOUT        PIC X(15) VALUE SPACES.                        
041700     03 MQ-IDPCPRT-JL2     PIC X(41) VALUE SPACES.                        
041800     03 MQ-LFE-SUFF2       PIC X(1)  VALUE ')'.                           
041900*                                                                         
042000 01  WS-IDBATCH.                                                          
042100     03  WS-IDLEVNR              PIC X(5).                                
042200     03  FILLER                  PIC X VALUE '0'.                         
042300     03  WS-BATCHNR              PIC X(12).                               
042400                                                                          
042500 01  WS3-KVQPACK.                                                         
042600     03  WS-TEXT-QTY             PIC X(5) VALUE 'QTY. '.                  
042700     03  WS2-KVQPACK             PIC X(5).                                
042800     03  WS-NUM-KVQPACK REDEFINES WS2-KVQPACK PIC 9(5).                   
042900     03  FILLER                  PIC X VALUE SPACE.                       
043000     03  WS-KDSORT               PIC X(2).                                
043100                                                                          
043200 01  WS-YEAR                     PIC 9(4).                                
043300                                                                          
043400 01  DAGENS-DATUM.                                                        
043500     03  DAGENS-YEAR             PIC 9(4).                                
043600     03  DAGENS-MAANAD           PIC 9(2).                                
043700                                                                          
043800 01  WS-DATUMKOD.                                                         
043900     03  WS-DATUMKOD-YEAR        PIC X.                                   
044000     03  WS-DATUMKOD-MAANAD      PIC X.                                   
044100                                                                          
044200 01  FINNS-DET-FLER-11-SEGMENT   PIC X       VALUE 'J'.                   
044300     88  INGA-FLER-11-SEGMENT                VALUE 'N'.                   
044400     88  FINNS-FLER-11-SEGMENT               VALUE 'J'.                   
044500                                                                          
044600 01  FINNS-DET-FLER-21-SEGMENT   PIC X       VALUE 'J'.                   
044700     88  INGA-FLER-21-SEGMENT                VALUE 'N'.                   
044800     88  FINNS-FLER-21-SEGMENT               VALUE 'J'.                   
044900                                                                          
045000 01  SSA1                        PIC X(64).                               
045100 01  SSA2                        PIC X(64).                               
045200 77  WS-KVQPACK                  PIC S9(5)   VALUE ZERO COMP-3.           
045300                                                                          
045400 77  IDLAYOUT-SW                 PIC X       VALUE 'J'.                   
045500     88  IDLAYOUT-OK                         VALUE 'J'.                   
045600     88  IDLAYOUT-FEL                        VALUE 'N'.                   
045700                                                                          
045800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
045900     88  NYCKLAR-OK                          VALUE 'J'.                   
046000     88  NYCKLAR-FEL                         VALUE 'N'.                   
046100                                                                          
046200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
046300     88  INDATA-OK                           VALUE 'J'.                   
046400     88  INDATA-FEL                          VALUE 'N'.                   
046500                                                                          
046600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
046700     88  EGEN-MID                            VALUE '6168'.                
046800     88  GODK-MID                            VALUE '6166' '6167'          
046900                                                   '6168'.                
047000     88  HELP-MID                            VALUE '0551'.                
047100     EJECT                                                                
047200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
047300 01  GENERELLA-SUBPROGRAM.                                                
047400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
047500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
047600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
047700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
047800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
047900     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
048000     03  WZ11OUTQ                PIC X(8)    VALUE 'WZ11OUTQ'.            
048100     EJECT                                                                
048200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
048300*01 -COPY WMEDAREA                                                        
048400     SKIP3                                                                
048500 01  MESSAGE-CODES.                                                       
048600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
048700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
048800     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
048900     03  TRYCK-PF4-FOR-PRINT     PIC X(3)    VALUE '081'.                 
049000     03  PF4-NO-INDATA           PIC X(3)    VALUE '231'.                 
049100     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
049200     03  WRONG-PRINTER           PIC X(3)    VALUE '772'.                 
049300     03  FEL-ANTAL               PIC X(3)    VALUE '302'.                 
049400     03  LAYOUT-SAKNAS           PIC X(3)    VALUE '149'.                 
049500     03  LAYOUT-FELAKTIG         PIC X(3)    VALUE '729'.                 
049600     03  INGET-PRINTAT           PIC X(3)    VALUE '167'.                 
049700                                                                          
049800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
049900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
050000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
050100                                                                          
050200*01  -COPY W400ARTU                                                       
050300     EJECT                                                                
050400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
050500*                                                                         
050600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
050700     SKIP3                                                                
050800*01 -COPY WMSGINIT                                                        
050900     EJECT                                                                
051000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
051100*                                                                         
051200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
051300     SKIP3                                                                
051400*01  MID -COPY W6I16801                                                   
051500     EJECT                                                                
051600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
051700     SKIP3                                                                
051800*01  -COPY WMSGAREA                                                       
051900     EJECT                                                                
052000     03  MOD REDEFINES MSG-AREA.                                          
052100*      05  -COPY W6O16801                                                 
052200                                                                          
052300 01  WS-RAD                      PIC X(250)  VALUE SPACE.                 
052400                                                                          
052500 01  FILLER                      PIC X(16)   VALUE 'OUTQ-AREA'.           
052600 01 OUTQ-AREA.                                                            
052700    03  OUTQ-CONTROL-AREA.                                                
052800        05  OUTQ-KDFUNC          PIC X(10).                               
052900        05  OUTQ-KDRC            PIC S9(9) COMP.                          
053000        05  OUTQ-IDCOM           PIC S9(9) COMP.                          
053100    03  OUTQ-OPEN-AREA.                                                   
053200        05  OUTQ-ADDISPABS       PIC X(50).                               
053300    03  OUTQ-PROPERTY-AREA.                                               
053400        05  OUTQ-PROPERTY-NAME   PIC X(100).                              
053500        05  OUTQ-PROPERTY-VALUE  PIC X(100).                              
053600    03  OUTQ-ADDITIONAL-INFO.                                             
053700        05  OUTQ-PHYSICALID      PIC X(100).                              
053800    03  OUTQ-KVDLEN              PIC S9(9) BINARY.                        
053900                                                                          
054000    03  OUTQ-DATA                PIC X(500)  VALUE SPACE.                 
054100                                                                          
054200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
054300     SKIP3                                                                
054400*01  -COPY WMFSAREA                                                       
054500     EJECT                                                                
054600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
054700*                                                                         
054800     EJECT                                                                
054900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
055000     SKIP3                                                                
055100 01  NYCKLAR-TILL-DLI.                                                    
055200     03  W-IDSKYLT-X.                                                     
055300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
055400     03  W-IDARTNR-X.                                                     
055500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
055600     03  W-IDPTYP-X.                                                      
055700         05  W-IDPTYP            PIC X(3)    VALUE 'R31'.                 
055800     SKIP2                                                                
055900*    --- STATUS-KOD FRÅN IMS                                              
056000 01  STATUS-WS                   PIC XX.                                  
056100     88  SEGMENT-FINNS                       VALUE '  '.                  
056200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
056300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
056400     SKIP2                                                                
056500 01  GODK-STATUSKODER.                                                    
056600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
056700     SKIP3                                                                
056800*    --- IMS FUNKTIONSKODER                                               
056900*01  -COPY W0003                                                          
057000     EJECT                                                                
057100*    ---  DLI INPUT-OUTPUT AREA                                           
057200                                                                          
057300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
057400 01  DLI-IO-WLBENA11.                                                     
057500*    03  -COPY WDD311                                                     
057600     EJECT                                                                
057700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLETIA01'.                    
057800 01  DLI-IO-WLETIA01.                                                     
057900*    03  -COPY WDK301                                                     
058000     EJECT                                                                
058100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLETIA11'.                    
058200 01  DLI-IO-WLETIA11.                                                     
058300*    03  -COPY WDK311                                                     
058400     EJECT                                                                
058500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
058600 01  DLI-IO-WLARTC01.                                                     
058700*    03  -COPY WDK601                                                     
058800     EJECT                                                                
058900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
059000 01  DLI-IO-WLARTC11.                                                     
059100*    03  -COPY WDK611                                                     
059200     EJECT                                                                
059300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINLE21'.                    
059400 01  DLI-IO-WLINLE21.                                                     
059500*    03  -COPY WDL221                                                     
059600     EJECT                                                                
059700 LINKAGE SECTION.                                                         
059800*01  -COPY W0009   -PRE MSG-                                              
059900     EJECT                                                                
060000*01  -COPY W0008   -PRE USEA-                                             
060100     05  FILLER                  PIC X.                                   
060200     EJECT                                                                
060300*01  -COPY W0008   -PRE ARTC-                                             
060400     05  FILLER                  PIC X.                                   
060500     EJECT                                                                
060600*01  -COPY W0008   -PRE BENA-                                             
060700     05  FILLER                  PIC X.                                   
060800     EJECT                                                                
060900*01  -COPY W0008   -PRE ETIA-                                             
061000     05  FILLER                  PIC X.                                   
061100     EJECT                                                                
061200*01  -COPY W0008   -PRE INLE-                                             
061300     05  FILLER                  PIC X.                                   
061400     EJECT                                                                
061500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTC-PCB                      
061600                           BENA-PCB ETIA-PCB INLE-PCB.                    
061700 MAIN SECTION.                                                            
061800                                                                          
061900     PERFORM IMS-GET-MSG                                                  
062000     IF SEGMENT-FINNS                                                     
062100       PERFORM A-INIT                                                     
062200       PERFORM B-KOLLA-NYCKLAR                                            
062300       IF NYCKLAR-OK                                                      
062400          IF MFS-UPDATE                                                   
062500             PERFORM F-LAES-VISA-INFO                                     
062600             PERFORM G-KOLLA-INPUT                                        
062700             IF INDATA-OK                                                 
062800                PERFORM H-STARTA-PRINT                                    
062900             END-IF                                                       
063000          ELSE                                                            
063100             IF MFS-FIRST                                                 
063200                PERFORM C-FOERSTA-SIDA                                    
063300             ELSE                                                         
063400                PERFORM E-SAMMA-SIDA                                      
063500             END-IF                                                       
063600             PERFORM F-LAES-VISA-INFO                                     
063700          END-IF                                                          
063800       END-IF                                                             
063900       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O16801 + 4                      
064000       PERFORM IMS-INSERT-MSG                                             
064100     END-IF                                                               
064200                                                                          
064300     MOVE ZERO TO RETURN-CODE                                             
064400     GOBACK                                                               
064500     .                                                                    
064600     EJECT                                                                
064700 A-INIT SECTION.                                                          
064800                                                                          
064900     IF MSG-DUBBLA-TRANSKODER                                             
065000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I16801                 
065100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
065200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
065300     ELSE                                                                 
065400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I16801                  
065500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
065600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
065700     END-IF                                                               
065800                                                                          
065900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
066000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
066100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
066200                                                                          
066300     MOVE LOW-VALUE  TO MSG-AREA                                          
066400     MOVE 'W6O168N1' TO MFS-IDMOD                                         
066500     MOVE '6168'     TO MOD-IDTRANS                                       
066600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
066700                                                                          
066800     IF MSGI-IDLAND-SPR = 'SE'                                            
066900        MOVE '0' TO MFS-KDHUVOMR                                          
067000     END-IF                                                               
067100                                                                          
067200     IF EGEN-MID OR HELP-MID                                              
067300       CONTINUE                                                           
067400     ELSE                                                                 
067500       MOVE SPACE TO MFS-KDTRTYP                                          
067600       MOVE '7' TO MFS-IDPFK                                              
067700     END-IF                                                               
067800     .                                                                    
067900     EJECT                                                                
068000 B-KOLLA-NYCKLAR SECTION.                                                 
068100                                                                          
068200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
068300     MOVE '001'             TO MSGI-KDCALL                                
068400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
068500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
068600     MOVE '6168'            TO MSGI-IDTRANS                               
068700     IF GODK-MID                                                          
068800        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
068900     END-IF                                                               
069000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
069100                                                                          
069200     MOVE JA TO NYCKLAR-SW                                                
069300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
069400     IF MID-IDARTNR-IN NOT = ALL '+'                                      
069500       MOVE '7'         TO MFS-IDPFK                                      
069600       MOVE SPACE       TO MFS-KDTRTYP                                    
069700     END-IF                                                               
069800     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
069900     IF MSGI-IDARTNR NUMERIC                                              
070000       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
070100     ELSE                                                                 
070200       MOVE NEJ TO NYCKLAR-SW                                             
070300     END-IF                                                               
070400     IF MSGI-IDLAND-SPR = 'SE'                                            
070500       MOVE 'S  ' TO MED-IDSKYLT                                          
070600                     W-IDSKYLT                                            
070700     ELSE                                                                 
070800       MOVE 'GB ' TO MED-IDSKYLT                                          
070900                     W-IDSKYLT                                            
071000     END-IF                                                               
071100                                                                          
071200     IF GODK-MID OR NYCKLAR-OK                                            
071300       MOVE MSGI-IDARTNR TO MOD-IDARTNR-UT                                
071400       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
071500     ELSE                                                                 
071600       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
071700     END-IF                                                               
071800                                                                          
071900     IF NYCKLAR-FEL                                                       
072000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
072100       CALL WMEDKONV USING MED-WMEDAREA                                   
072200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
072300       PERFORM MFS-RENSA-FAELT-IN                                         
072400       PERFORM MFS-RENSA-FAELT-UT                                         
072500     END-IF                                                               
072600     .                                                                    
072700     EJECT                                                                
072800 C-FOERSTA-SIDA SECTION.                                                  
072900                                                                          
073000     PERFORM MFS-RENSA-FAELT-IN                                           
073100     .                                                                    
073200     EJECT                                                                
073300 E-SAMMA-SIDA SECTION.                                                    
073400                                                                          
073500     IF EGEN-MID OR HELP-MID                                              
073600       IF MID-KVANTAL-ETIK   = ALL '+'                                    
073700       AND MID-IDPRTLST      = ALL '+'                                    
073800       AND MID-BEARTURS-JUST = ALL '+'                                    
073900       AND MID-KVQPACK-JUST  = ALL '+'                                    
074000       AND MID-IDBATCH       = ALL '+'                                    
074100          PERFORM MFS-RENSA-FAELT-IN                                      
074200       ELSE                                                               
074300          MOVE TRYCK-PF4-FOR-PRINT TO MED-IDMFSINF                        
074400          CALL WMEDKONV USING MED-WMEDAREA                                
074500          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
074600          PERFORM EA-MID-INDATA-TILL-MOD                                  
074700       END-IF                                                             
074800     ELSE                                                                 
074900       PERFORM MFS-RENSA-FAELT-IN                                         
075000     END-IF                                                               
075100     .                                                                    
075200     EJECT                                                                
075300 EA-MID-INDATA-TILL-MOD SECTION.                                          
075400                                                                          
075500     IF MID-KVANTAL-ETIK NOT = ALL '+'                                    
075600        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVANTAL-ETIK-ATTR               
075700        MOVE MFS-ROER-EJ-FAELT     TO MOD-KVANTAL-ETIK-IN                 
075800     ELSE                                                                 
075900        MOVE MFS-RENSA-FAELT TO MOD-KVANTAL-ETIK-IN                       
076000     END-IF                                                               
076100                                                                          
076200     IF MID-IDPRTLST NOT = ALL '+'                                        
076300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRTLST-ATTR                   
076400        MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPRTLST-IN                     
076500     ELSE                                                                 
076600        MOVE MFS-RENSA-FAELT TO MOD-IDPRTLST-IN                           
076700     END-IF                                                               
076800                                                                          
076900     IF MID-BEARTURS-JUST NOT = ALL '+'                                   
077000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEARTURS-JUST-ATTR              
077100        MOVE MFS-ROER-EJ-FAELT     TO MOD-BEARTURS-JUST-IN                
077200     ELSE                                                                 
077300        MOVE MFS-RENSA-FAELT TO MOD-BEARTURS-JUST-IN                      
077400     END-IF                                                               
077500                                                                          
077600     IF MID-KVQPACK-JUST NOT = ALL '+'                                    
077700        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVQPACK-JUST-ATTR               
077800        MOVE MFS-ROER-EJ-FAELT     TO MOD-KVQPACK-JUST-IN                 
077900     ELSE                                                                 
078000        MOVE MFS-RENSA-FAELT TO MOD-KVQPACK-JUST-IN                       
078100     END-IF                                                               
078200                                                                          
078300     IF MID-IDBATCH NOT = ALL '+'                                         
078400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDBATCH-ATTR                    
078500        MOVE MFS-ROER-EJ-FAELT     TO MOD-IDBATCH-IN                      
078600     ELSE                                                                 
078700        MOVE MFS-RENSA-FAELT TO MOD-IDBATCH-IN                            
078800     END-IF                                                               
078900     .                                                                    
079000     EJECT                                                                
079100 F-LAES-VISA-INFO SECTION.                                                
079200                                                                          
079300     PERFORM FA-LAES-GRUNDDATA                                            
079400                                                                          
079500     IF SEGMENT-SAKNAS                                                    
079600        MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                               
079700        CALL WMEDKONV USING MED-WMEDAREA                                  
079800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
079900        PERFORM MFS-RENSA-FAELT-UT                                        
080000     ELSE                                                                 
080100        PERFORM FB-VISA-ARTINF                                            
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
080500 FA-LAES-GRUNDDATA SECTION.                                               
080600                                                                          
080700     PERFORM IMS-GET-ETIA01                                               
080800     .                                                                    
080900     EJECT                                                                
081000 FB-VISA-ARTINF SECTION.                                                  
081100                                                                          
081200     MOVE ETI-IDUSER       TO MOD-IDUSER                                  
081300     MOVE ETI-TIUPPDAT     TO MOD-TIUPPDAT                                
081400     MOVE ETI-IDLAYOUT     TO MOD-IDLAYOUT                                
081500     MOVE ETI-TEETIK-INT   TO MOD-TEETIK-INT                              
081600     MOVE ETI-IDARTNR-ETIK TO MOD-IDARTNR-ETIK                            
081700                                                                          
081800     PERFORM IMS-GET-BENA11                                               
081900     MOVE TEXT-BEART       TO MOD-BEART                                   
082000                                                                          
082100     PERFORM IMS-GET-ARTC01                                               
082200     MOVE ART-KDSORT       TO MOD-KDSORT                                  
082300     PERFORM IMS-GET-ARTC11                                               
082400     MOVE CLAG-KVQPACK-0   TO MOD-KVQPACK-0                               
082500     MOVE CLAG-KVQPACK-1   TO MOD-KVQPACK-1                               
082600     MOVE CLAG-KVQPACK-2   TO MOD-KVQPACK-2                               
082700*                                                                         
082800     IF CLAG-KVQPACK-0 = 0 AND CLAG-KVQPACK-1 = 0 AND                     
082900        CLAG-KVQPACK-2 = 0                                                
083000        MOVE 1  TO WS-KVQPACK                                             
083100     ELSE                                                                 
083200        IF CLAG-KVQPACK-0 < CLAG-KVQPACK-1 AND                            
083300           CLAG-KVQPACK-0 > 0                                             
083400           IF CLAG-KVQPACK-0 < CLAG-KVQPACK-2                             
083500              MOVE CLAG-KVQPACK-0 TO WS-KVQPACK                           
083600           ELSE                                                           
083700              IF CLAG-KVQPACK-2 > 0                                       
083800                 MOVE CLAG-KVQPACK-2 TO WS-KVQPACK                        
083900              ELSE                                                        
084000                 MOVE CLAG-KVQPACK-1 TO WS-KVQPACK                        
084100              END-IF                                                      
084200           END-IF                                                         
084300        ELSE                                                              
084400           IF CLAG-KVQPACK-1 < CLAG-KVQPACK-2 AND                         
084500              CLAG-KVQPACK-1 > 0                                          
084600              MOVE CLAG-KVQPACK-1 TO WS-KVQPACK                           
084700           ELSE                                                           
084800              IF CLAG-KVQPACK-1 > 0                                       
084900                 IF CLAG-KVQPACK-2 > 0                                    
085000                    MOVE CLAG-KVQPACK-2 TO WS-KVQPACK                     
085100                 ELSE                                                     
085200                    MOVE CLAG-KVQPACK-1 TO WS-KVQPACK                     
085300                 END-IF                                                   
085400              ELSE                                                        
085500                 IF CLAG-KVQPACK-0 = 0                                    
085600                    MOVE CLAG-KVQPACK-2 TO WS-KVQPACK                     
085700                 ELSE                                                     
085800                    IF CLAG-KVQPACK-2 = 0                                 
085900                       MOVE CLAG-KVQPACK-0 TO WS-KVQPACK                  
086000                    ELSE                                                  
086100                       IF CLAG-KVQPACK-0 < CLAG-KVQPACK-2                 
086200                          MOVE CLAG-KVQPACK-0 TO WS-KVQPACK               
086300                       ELSE                                               
086400                          MOVE CLAG-KVQPACK-2 TO WS-KVQPACK               
086500                       END-IF                                             
086600                    END-IF                                                
086700                 END-IF                                                   
086800              END-IF                                                      
086900           END-IF                                                         
087000        END-IF                                                            
087100     END-IF                                                               
087200*                                                                         
087300     MOVE WS-KVQPACK        TO MOD-KVQPACK-JUST                           
087400     MOVE CLAG-KDARTURS     TO ARTU-KDARTURS                              
087500     MOVE SPACE             TO ARTU-IDDC                                  
087600     MOVE ZERO              TO ARTU-IDDISTR                               
087700     CALL W400ARTU USING ARTU-W400ARTU                                    
087800     MOVE ARTU-BEARTURS-ENG TO MOD-BEARTURS                               
087900                                                                          
088000     IF MID-IDBATCH = ALL '+'                                             
088100        PERFORM FC-HAEMTA-IDBATCHNR                                       
088200     END-IF                                                               
088300     MOVE MID-IDBATCH TO MQ-IDBATCH                                       
088400     .                                                                    
088500     EJECT                                                                
088600 FC-HAEMTA-IDBATCHNR SECTION.                                             
088700     MOVE ART-IDLEVNR   TO WS-IDLEVNR                                     
088800                                                                          
088900     PERFORM IMS-GN-INLE11                                                
089000     IF SEGMENT-FINNS                                                     
089100        PERFORM IMS-GNP-INLE21                                            
089200        IF SEGMENT-FINNS                                                  
089300           MOVE  MOT-IDLOPNRM TO WS-BATCHNR                               
089400           PERFORM FD-FINNS-DET-FLER-R31OR                                
089500        ELSE                                                              
089600           MOVE SPACE TO WS-IDBATCH                                       
089700        END-IF                                                            
089800     ELSE                                                                 
089900        MOVE SPACE TO WS-IDBATCH                                          
090000     END-IF                                                               
090100                                                                          
090200     MOVE WS-IDBATCH TO MID-IDBATCH                                       
090300     MOVE WS-IDBATCH TO MOD-IDBATCH-IN                                    
090400     .                                                                    
090500     EJECT                                                                
090600 FD-FINNS-DET-FLER-R31OR SECTION.                                         
090700     PERFORM IMS-GNP-INLE21                                               
090800     IF SEGMENT-FINNS                                                     
090900* I DETTA FALL FINNS DET FLER ÄN EN R31:A INGÅENDE                        
091000        MOVE SPACE TO WS-IDBATCH                                          
091100     ELSE                                                                 
091200        PERFORM FE-KOLLA-ANDRA-11-SEGMENT                                 
091300     END-IF                                                               
091400     .                                                                    
091500     EJECT                                                                
091600 FE-KOLLA-ANDRA-11-SEGMENT SECTION.                                       
091700     PERFORM IMS-GN-INLE11                                                
091800     PERFORM UNTIL INGA-FLER-11-SEGMENT                                   
091900        IF SEGMENT-FINNS                                                  
092000           PERFORM IMS-GNP-INLE21                                         
092100           PERFORM UNTIL INGA-FLER-21-SEGMENT                             
092200              IF SEGMENT-FINNS                                            
092300                 MOVE SPACE TO WS-IDBATCH                                 
092400                 MOVE NEJ TO FINNS-DET-FLER-11-SEGMENT                    
092500              END-IF                                                      
092600              MOVE NEJ TO FINNS-DET-FLER-21-SEGMENT                       
092700           END-PERFORM                                                    
092800             PERFORM IMS-GN-INLE11                                        
092900        ELSE                                                              
093000           MOVE NEJ TO FINNS-DET-FLER-11-SEGMENT                          
093100        END-IF                                                            
093200     END-PERFORM                                                          
093300     .                                                                    
093400     EJECT                                                                
093500 G-KOLLA-INPUT SECTION.                                                   
093600                                                                          
093700     MOVE JA  TO INDATA-SW                                                
093800     IF MID-KVANTAL-ETIK     = ALL '+'                                    
093900     AND MID-IDPRTLST        = ALL '+'                                    
094000     AND MID-BEARTURS-JUST   = ALL '+'                                    
094100     AND MID-KVQPACK-JUST    = ALL '+'                                    
094200     AND MID-IDBATCH         = ALL '+'                                    
094300        MOVE PF4-NO-INDATA TO MED-IDMFSFEL                                
094400        CALL WMEDKONV USING MED-WMEDAREA                                  
094500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
094600        PERFORM MFS-ROER-EJ-FAELT-IN                                      
094700        PERFORM MFS-ROER-EJ-FAELT-UT                                      
094800        MOVE NEJ TO INDATA-SW                                             
094900     ELSE                                                                 
095000*                                                                         
095100*   KONTROLLERA PRINTERS !                                                
095200*       (TILLDELA NT-PRINTERADRESS TILL MQ-TRANS)                         
095300*                                                                         
095400       IF MID-IDPRTLST NOT = ALL '+'                                      
095500         SET PRINTER-IX TO +1                                             
095600         SEARCH GODK-PRINTER                                              
095700           AT END                                                         
095800             MOVE WRONG-PRINTER TO MED-IDMFSFEL                           
095900             MOVE NEJ TO INDATA-SW                                        
096000             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRTLST-ATTR                 
096100           WHEN WS-IDPRTLST (PRINTER-IX) = MID-IDPRTLST                   
096200             MOVE WS-IDPCPRT (PRINTER-IX) TO MQ-IDPCPRT-JL2               
096300             MOVE WS-PRINTERTYP (PRINTER-IX) TO WS-SHOWKOD                
096400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRTLST-ATTR               
096500         END-SEARCH                                                       
096600       ELSE                                                               
096700         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPRTLST-ATTR                   
096800         MOVE NEJ TO INDATA-SW                                            
096900       END-IF                                                             
097000*                                                                         
097100*   KONTROLLERA URSPRUNG !                                                
097200*       (TILLDELA URSPRUNG ELLER JUST-URSPRUNG TILL MQ-TRANS)             
097300*                                                                         
097400       IF MID-BEARTURS-JUST NOT = ALL '+'                                 
097500          IF MID-BEARTURS-JUST NOT = SPACE                                
097600             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEARTURS-JUST-ATTR          
097700             MOVE MID-BEARTURS-JUST TO MQ-BEARTURS                        
097800          ELSE                                                            
097900             MOVE ARTU-BEARTURS-ENG TO MQ-BEARTURS                        
098000          END-IF                                                          
098100       ELSE                                                               
098200          MOVE ARTU-BEARTURS-ENG    TO MQ-BEARTURS                        
098300       END-IF                                                             
098400*                                                                         
098500*   KONTROLLERA ANTAL !                                                   
098600*       (TILLDELA ANTAL TILL MQ-TRANS)                                    
098700*                                                                         
098800       IF MID-KVANTAL-ETIK NOT = ALL '+'                                  
098900         IF MID-KVANTAL-ETIK NOT NUMERIC                                  
099000           MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ETIK-ATTR                
099100           MOVE NEJ TO INDATA-SW                                          
099200           MOVE FEL-ANTAL    TO MED-IDMFSFEL                              
099300         ELSE                                                             
099400           IF MID-KVANTAL-ETIK > ZERO                                     
099500             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVANTAL-ETIK-ATTR            
099600           ELSE                                                           
099700              MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ETIK-ATTR             
099800              MOVE NEJ TO INDATA-SW                                       
099900              MOVE FEL-ANTAL TO MED-IDMFSFEL                              
100000           END-IF                                                         
100100         END-IF                                                           
100200       ELSE                                                               
100300          MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ETIK-ATTR                 
100400          MOVE NEJ TO INDATA-SW                                           
100500          MOVE FEL-ANTAL     TO MED-IDMFSFEL                              
100600       END-IF                                                             
100700*                                                                         
100800*   KONTROLLERA JUSTERAD KVANT                                            
100900*       (TILLDELA RÄTT KVANT TILL MQ-TRANS)                               
101000*                                                                         
101100       IF MID-KVQPACK-JUST NOT = ALL '+'                                  
101200         IF MID-KVQPACK-JUST NOT NUMERIC                                  
101300           MOVE MFS-NUM-FAELT-FEL TO MOD-KVQPACK-JUST-ATTR                
101400           MOVE NEJ TO INDATA-SW                                          
101500         ELSE                                                             
101600           IF MID-KVQPACK-JUST > ZERO                                     
101700              MOVE MFS-NUM-FAELT-RAETT TO MOD-KVQPACK-JUST-ATTR           
101800              MOVE MID-KVQPACK-JUST TO  WS2-KVQPACK                       
101900           ELSE                                                           
102000              MOVE MFS-NUM-FAELT-FEL TO MOD-KVQPACK-JUST-ATTR             
102100              MOVE NEJ TO INDATA-SW                                       
102200           END-IF                                                         
102300         END-IF                                                           
102400       ELSE                                                               
102500         MOVE WS-KVQPACK   TO   WS2-KVQPACK                               
102600       END-IF                                                             
102700       IF INDATA-OK                                                       
102800          IF ART-KDSORT = 'ST' OR                                         
102900             ART-KDSORT = 'SA' OR                                         
103000             ART-KDSORT = 'PA'                                            
103100             MOVE SPACE TO WS-KDSORT                                      
103200          ELSE                                                            
103300             MOVE ART-KDSORT TO WS-KDSORT                                 
103400          END-IF                                                          
103500                                                                          
103600          IF WS-NUM-KVQPACK > 1                                           
103700             INSPECT WS2-KVQPACK REPLACING LEADING ZEROES BY SPACE        
103800             MOVE WS3-KVQPACK TO MQ-KVQPACK                               
103900          ELSE                                                            
104000             IF ART-KDSORT = 'ST' OR                                      
104100                ART-KDSORT = 'SA' OR                                      
104200                ART-KDSORT = 'PA'                                         
104300                MOVE SPACE TO MQ-KVQPACK                                  
104400             ELSE                                                         
104500             INSPECT WS2-KVQPACK REPLACING LEADING ZEROES BY SPACE        
104600                MOVE WS3-KVQPACK TO MQ-KVQPACK                            
104700             END-IF                                                       
104800          END-IF                                                          
104900       END-IF                                                             
105000*                                                                         
105100*   KONTROLLERA BATCHNUMMER                                               
105200*       (TILLDELA BATCHNR TILL MQ-TRANS)                                  
105300*                                                                         
105400       IF MID-IDBATCH  NOT = ALL '+'                                      
105500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDBATCH-ATTR                    
105600       ELSE                                                               
105700         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDBATCH-ATTR                    
105800         MOVE NEJ TO INDATA-SW                                            
105900       END-IF                                                             
106000*                                                                         
106100*   KONTROLLERA IDLAYOUT (OBS. EJ FRÅN MID !!!)                           
106200*       (TILLDELA LAYOUT TILL MQ-TRANS)                                   
106300*                                                                         
106400*       EN KORREKT IDLAYOUT ÄR UPPBYGGD SÅ HÄR;                           
106500*          2 STAN 01 B U                                                  
106600*          3 SPEC 01 R L                                                  
106700*          4 SATS 05 B L                                                  
106800*          ¦  ¦   ¦  ¦ ¦                                                  
106900*          ¦  ¦   ¦  ¦ -- (U/L) MED/UTAN LOGO                             
107000*          ¦  ¦   ¦  ---- (B/R  BANA / RULL SKRIVARE                      
107100*          ¦  ¦   ------- LÖPNUMMER                                       
107200*          ¦  ----------- (STAN/SPEC/SATS/TEST) TYP AV LAYOUT             
107300*          -------------- (1/2/3/4/7/9) STORLEK AV ETIKETT                
107400*                                                                         
107500*                                                                         
107600       IF ETI-IDLAYOUT = SPACE                                            
107700          MOVE LAYOUT-SAKNAS        TO MED-IDMFSFEL                       
107800          MOVE NEJ TO INDATA-SW                                           
107900       ELSE                                                               
108000          MOVE JA TO IDLAYOUT-SW                                          
108100          MOVE ETI-IDLAYOUT      TO MQ-IDLAYOUT-PRE                       
108200                                                                          
108300          IF WS-STORLEK-KOD IS NOT NUMERIC                                
108400             MOVE NEJ TO IDLAYOUT-SW                                      
108500          END-IF                                                          
108600                                                                          
108700          IF WS-STORLEK-KOD = '1' AND WS-LOGO-KOD = 'L'                   
108800             MOVE NEJ TO IDLAYOUT-SW                                      
108900          END-IF                                                          
109000          IF WS-STORLEK-KOD = '2' AND WS-LOGO-KOD = 'L'                   
109100             MOVE NEJ TO IDLAYOUT-SW                                      
109200          END-IF                                                          
109300          IF WS-STORLEK-KOD = '3' AND WS-LOGO-KOD = 'U'                   
109400             MOVE NEJ TO IDLAYOUT-SW                                      
109500          END-IF                                                          
109600                                                                          
109700          IF WS-LOEPNUMMER IS NOT NUMERIC                                 
109800             MOVE NEJ TO IDLAYOUT-SW                                      
109900          END-IF                                                          
110000                                                                          
110100          IF WS-TYP-KOD = 'STAN' OR 'SPEC' OR 'SATS' OR 'TEST'            
110200             CONTINUE                                                     
110300          ELSE                                                            
110400             MOVE NEJ TO IDLAYOUT-SW                                      
110500          END-IF                                                          
110600                                                                          
110700          IF WS-TOMRUM NOT = SPACE                                        
110800             MOVE NEJ TO IDLAYOUT-SW                                      
110900          END-IF                                                          
111000                                                                          
111100          IF IDLAYOUT-FEL                                                 
111200            MOVE LAYOUT-FELAKTIG   TO MED-IDMFSFEL                        
111300            MOVE NEJ TO INDATA-SW                                         
111400          ELSE                                                            
111500            MOVE SPACE             TO WS-TOMRUM                           
111600          END-IF                                                          
111700       END-IF                                                             
111800*                                                                         
111900*  LÄGG UT FELMEDDELANDE OM SÅ BEHÖVS                                     
112000*                                                                         
112100       IF INDATA-FEL                                                      
112200          IF MED-IDMFSFEL = SPACE                                         
112300             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
112400          END-IF                                                          
112500          CALL WMEDKONV USING MED-WMEDAREA                                
112600          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
112700          PERFORM MFS-ROER-EJ-FAELT-UT                                    
112800          PERFORM MFS-ROER-EJ-FAELT-IN                                    
112900       ELSE                                                               
113000* ALLT GICK OK, FLYTTA UTDATA TILL MQ-AREA                                
113100          MOVE W-IDARTNR         TO WS-IDARTNR1                           
113200          MOVE MID-KVANTAL-ETIK  TO MQ-KVANTAL                            
113300          PERFORM GA-HAEMTA-ALLA-TEETIK-EXT                               
113400       END-IF                                                             
113500     END-IF                                                               
113600     .                                                                    
113700     EJECT                                                                
113800 GA-HAEMTA-ALLA-TEETIK-EXT SECTION.                                       
113900     PERFORM IMS-GNP-ETIA11                                               
114000     PERFORM UNTIL SEGMENT-SAKNAS                                         
114100        EVALUATE SPEC-IDRADNR                                             
114200          WHEN 1  MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-01                
114300          WHEN 2  MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-02                
114400          WHEN 3  MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-03                
114500          WHEN 4  MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-04                
114600          WHEN 5  MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-05                
114700          WHEN 6  MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-06                
114800          WHEN 7  MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-07                
114900          WHEN 8  MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-08                
115000          WHEN 9  MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-09                
115100          WHEN 10 MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-10                
115200          WHEN 11 MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-11                
115300          WHEN 12 MOVE SPEC-TEETIK-EXT TO MQ-TEETIK-EXT-12                
115400        END-EVALUATE                                                      
115500        PERFORM IMS-GNP-ETIA11                                            
115600     END-PERFORM                                                          
115700     .                                                                    
115800     EJECT                                                                
115900 H-STARTA-PRINT SECTION.                                                  
116000                                                                          
116100     PERFORM H-SKAPA-DATUMKOD                                             
116200                                                                          
116300     PERFORM HA-INIT-MQ                                                   
116400     PERFORM VARYING MQ-IX FROM +1 BY +1                                  
116500       UNTIL MQ-IX > MAX-MQ-IX                                            
116600       MOVE MQ-AREA (MQ-IX) TO WS-RAD                                     
116700       PERFORM HB-SEND-MQ                                                 
116800     END-PERFORM                                                          
116900     PERFORM HC-FINISH-MQ                                                 
117000                                                                          
117100     .                                                                    
117200     EJECT                                                                
117300 H-SKAPA-DATUMKOD SECTION.                                                
117400*    OM DENNA FUNKTION EJ LÄNGRE FUNGERAR BETYDER DET ATT                 
117500*    PROGRAMMET FORTFARANDE ÄR I GÅNG ÅR 2070.                            
117600*    DÅ ÄR DET DAGS ATT NÅGON TAR SIG EN TITT PÅ DET ;-)                  
117700                                                                          
117800     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
117900     COMPUTE WS-YEAR = DAGENS-YEAR - 1979                                 
118000     IF WS-YEAR > 59                                                      
118100        COMPUTE WS-YEAR = WS-YEAR - 60                                    
118200     ELSE                                                                 
118300        IF WS-YEAR > 29                                                   
118400           COMPUTE WS-YEAR = WS-YEAR - 30                                 
118500        END-IF                                                            
118600     END-IF                                                               
118700                                                                          
118800     EVALUATE WS-YEAR                                                     
118900     WHEN 01 MOVE 'A' TO WS-DATUMKOD-YEAR                                 
119000     WHEN 02 MOVE 'B' TO WS-DATUMKOD-YEAR                                 
119100     WHEN 03 MOVE 'C' TO WS-DATUMKOD-YEAR                                 
119200     WHEN 04 MOVE 'D' TO WS-DATUMKOD-YEAR                                 
119300     WHEN 05 MOVE 'E' TO WS-DATUMKOD-YEAR                                 
119400     WHEN 06 MOVE 'F' TO WS-DATUMKOD-YEAR                                 
119500     WHEN 07 MOVE 'G' TO WS-DATUMKOD-YEAR                                 
119600     WHEN 08 MOVE 'H' TO WS-DATUMKOD-YEAR                                 
119700     WHEN 09 MOVE 'J' TO WS-DATUMKOD-YEAR                                 
119800     WHEN 10 MOVE 'K' TO WS-DATUMKOD-YEAR                                 
119900     WHEN 11 MOVE 'L' TO WS-DATUMKOD-YEAR                                 
120000     WHEN 12 MOVE 'M' TO WS-DATUMKOD-YEAR                                 
120100     WHEN 13 MOVE 'N' TO WS-DATUMKOD-YEAR                                 
120200     WHEN 14 MOVE 'P' TO WS-DATUMKOD-YEAR                                 
120300     WHEN 15 MOVE 'R' TO WS-DATUMKOD-YEAR                                 
120400     WHEN 16 MOVE 'S' TO WS-DATUMKOD-YEAR                                 
120500     WHEN 17 MOVE 'T' TO WS-DATUMKOD-YEAR                                 
120600     WHEN 18 MOVE 'V' TO WS-DATUMKOD-YEAR                                 
120700     WHEN 19 MOVE 'W' TO WS-DATUMKOD-YEAR                                 
120800     WHEN 20 MOVE 'X' TO WS-DATUMKOD-YEAR                                 
120900     WHEN 21 MOVE 'Y' TO WS-DATUMKOD-YEAR                                 
121000     WHEN 22 MOVE '1' TO WS-DATUMKOD-YEAR                                 
121100     WHEN 23 MOVE '2' TO WS-DATUMKOD-YEAR                                 
121200     WHEN 24 MOVE '3' TO WS-DATUMKOD-YEAR                                 
121300     WHEN 25 MOVE '4' TO WS-DATUMKOD-YEAR                                 
121400     WHEN 26 MOVE '5' TO WS-DATUMKOD-YEAR                                 
121500     WHEN 27 MOVE '6' TO WS-DATUMKOD-YEAR                                 
121600     WHEN 28 MOVE '7' TO WS-DATUMKOD-YEAR                                 
121700     WHEN 29 MOVE '8' TO WS-DATUMKOD-YEAR                                 
121800     WHEN  0 MOVE '9' TO WS-DATUMKOD-YEAR                                 
121900     WHEN OTHER MOVE '0' TO WS-DATUMKOD-YEAR                              
122000     END-EVALUATE                                                         
122100                                                                          
122200     EVALUATE DAGENS-MAANAD                                               
122300     WHEN 01 MOVE 'A' TO WS-DATUMKOD-MAANAD                               
122400     WHEN 02 MOVE 'B' TO WS-DATUMKOD-MAANAD                               
122500     WHEN 03 MOVE 'C' TO WS-DATUMKOD-MAANAD                               
122600     WHEN 04 MOVE 'D' TO WS-DATUMKOD-MAANAD                               
122700     WHEN 05 MOVE 'E' TO WS-DATUMKOD-MAANAD                               
122800     WHEN 06 MOVE 'F' TO WS-DATUMKOD-MAANAD                               
122900     WHEN 07 MOVE 'G' TO WS-DATUMKOD-MAANAD                               
123000     WHEN 08 MOVE 'H' TO WS-DATUMKOD-MAANAD                               
123100     WHEN 09 MOVE 'J' TO WS-DATUMKOD-MAANAD                               
123200     WHEN 10 MOVE 'K' TO WS-DATUMKOD-MAANAD                               
123300     WHEN 11 MOVE 'L' TO WS-DATUMKOD-MAANAD                               
123400     WHEN 12 MOVE 'M' TO WS-DATUMKOD-MAANAD                               
123500     END-EVALUATE                                                         
123600                                                                          
123700     MOVE WS-DATUMKOD TO MQ-DATUMKOD                                      
123800     .                                                                    
123900     EJECT                                                                
124000 HA-INIT-MQ SECTION.                                                      
124100                                                                          
124200     PERFORM HE-TA-BORT-NOLLOR                                            
124300                                                                          
124400*    I PRINTERIET SKALL DET VARA MÖJLIGT ATT SKRIVA                       
124500*    UT ETIKETTER UTAN DATUMKOD. L&F EASE KAN MHA. ETT                    
124600*    DDE-KOMMANDO (SETSHOWFIELD) GÖMMA FÄLT SÅ ATT                        
124700*    DE INTE SKRIVS UT PÅ ETIKETTEN.                                      
124800*                                                                         
124900*    I PRINTERIET (SKRIVARE 584...) FINNS MÖJLIGHET                       
125000*    ATT NÄR MAN ANGER SKRIVARE LÄGGA TILL ETT 'X' EFTER                  
125100*    SKRIVARNAMNET. DETTA BETYDER ATT INGEN DATUMKOD SKALL                
125200*    UT PÅ ETIKETTEN, VARAV 'FIXEN' HÄR NEDAN.                            
125300*                                                                         
125400     IF WS-SHOWKOD = '5XD' OR '8XD'                                       
125500        MOVE 1 TO MQ-SHOW                                                 
125600     ELSE                                                                 
125700        MOVE 3 TO MQ-SHOW                                                 
125800     END-IF                                                               
125900                                                                          
126000*    OM MAN SKALL SKRIVA UT EN ETIKETT PÅ EN SMALBREDDS-                  
126100*    SKRIVARE (572:A) MÅSTE LAYOUTEN VARA DESIGNAD FÖR                    
126200*    EN SÅDAN SKRIVARE.                                                   
126300*                                                                         
126400*    NÄR PRINTERIET SKAPAR LAYOUTER GÖR DET DUBBLETTER FÖR                
126500*    ALLA LAYOUTER (EN FÖR 572:AN OCH EN FÖR 872:AN) VILKET               
126600*    GÖR DET MÖJLIGT ATT SKRIVA UT "SAMMA" ETIKETT PÅ BÅDE                
126700*    572:AN OCH 872:AN.                                                   
126800*                                                                         
126900*    EFTERSOM PRINTERIETS SKALL FÖLJA EN STRIKT NAMNSTANDARD              
127000*    ÄR DET MÖJLIGT ATT 'OVERRIDA' IDLAYOUT-FÄLTET ENLIGT                 
127100*    NEDAN.                                                               
127200*                                                                         
127300                                                                          
127400*171031 AN EXCLUSION BEING MADE HERE FOR THE LABEL 6STAN01BL/RL           
127500                                                                          
127600     IF MQ-IDLAYOUT-PRE = '6STAN01BL' OR '6STAN01RL'                      
127700       CONTINUE                                                           
127800     ELSE                                                                 
127900       IF WS-SKRIVARE = '5'                                               
128000          MOVE 'R' TO WS-SKRIVARE-KOD                                     
128100       ELSE                                                               
128200          MOVE 'B' TO WS-SKRIVARE-KOD                                     
128300       END-IF                                                             
128400     END-IF                                                               
128500                                                                          
128600     MOVE SPACES                 TO MQ-IDLAYOUT                           
128700     STRING MQ-IDLAYOUT-PRE   DELIMITED BY SPACE                          
128800            MQ-LFE-SUFF       DELIMITED BY SIZE                           
128900            INTO MQ-IDLAYOUT                                              
129000     END-STRING                                                           
129100     MOVE SPACES                 TO MQ-LAYOUT                             
129200     STRING MQ-IDLAYOUT       DELIMITED BY SPACE                          
129300            MQ-LFE-SUFF2      DELIMITED BY SIZE                           
129400            INTO MQ-LAYOUT                                                
129500                                                                          
129600     STRING MQ-IDPCPRT-JL2    DELIMITED BY SPACE                          
129700            MQ-LFE-SUFF2      DELIMITED BY SIZE                           
129800     '                                         ' DELIMITED BY SIZE        
129900            INTO MQ-IDPCPRT-JL1                                           
130000            ON OVERFLOW CONTINUE                                          
130100     END-STRING                                                           
130200                                                                          
130300     PERFORM S01-OUTQ-OPEN                                                
130400                                                                          
130500     MOVE 'x_interform_metadata_com_interform400_xml_txt2xml'             
130600                                 TO OUTQ-PROPERTY-NAME                    
130700     MOVE 'true'                 TO OUTQ-PROPERTY-VALUE                   
130800     PERFORM S04-SET-PROP                                                 
130900                                                                          
131000     MOVE 'x_interform_metadata_applicationName'                          
131100                                 TO OUTQ-PROPERTY-NAME                    
131200     MOVE 'PULS'                 TO OUTQ-PROPERTY-VALUE                   
131300     PERFORM S04-SET-PROP                                                 
131400                                                                          
131500     MOVE 'x_interform_metadata_interform_destination_printer_prin        
131600-         'terName'              TO OUTQ-PROPERTY-NAME                    
131700     MOVE MQ-IDPCPRT-JL2         TO OUTQ-PROPERTY-VALUE                   
131800     PERFORM S04-SET-PROP                                                 
131900                                                                          
132000     MOVE 'x_interform_metadata_printTemplate'                            
132100                                 TO OUTQ-PROPERTY-NAME                    
132200     MOVE MQ-IDLAYOUT            TO OUTQ-PROPERTY-VALUE                   
132300     PERFORM S04-SET-PROP                                                 
132400     .                                                                    
132500                                                                          
132600 HB-SEND-MQ SECTION.                                                      
132700                                                                          
132800     MOVE +83                    TO OUTQ-KVDLEN                           
132900     PERFORM S02-OUTQ-PUT                                                 
133000     .                                                                    
133100                                                                          
133200 HC-FINISH-MQ SECTION.                                                    
133300                                                                          
133400     PERFORM S03-OUTQ-CLOSE                                               
133500                                                                          
133600     MOVE INF-PRINT-BEGAERD    TO    MED-IDMFSINF                         
133700     CALL WMEDKONV             USING MED-WMEDAREA                         
133800     MOVE MED-MFSINF           TO    MOD-TEMFSINF                         
133900                                                                          
134000     PERFORM HD-ORDNA-FORMAT                                              
134100     .                                                                    
134200                                                                          
134300 HD-ORDNA-FORMAT SECTION.                                                 
134400     MOVE MFS-RENSA-FAELT       TO MOD-KVANTAL-ETIK-IN                    
134500                                   MOD-KVQPACK-JUST-IN                    
134600                                   MOD-IDARTNR-UT                         
134700                                                                          
134800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRTLST-ATTR                      
134900                                   MOD-BEARTURS-JUST-ATTR                 
135000                                   MOD-IDBATCH-ATTR                       
135100                                                                          
135200     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDPRTLST-IN                        
135300                                   MOD-BEARTURS-JUST-IN                   
135400                                   MOD-IDBATCH-IN                         
135500     .                                                                    
135600                                                                          
135700 HE-TA-BORT-NOLLOR SECTION.                                               
135800                                                                          
135900     STRING   FUNCTION TRIM (WS-IDARTNR-GRP)                              
136000                                DELIMITED BY SIZE                         
136100              ')'               DELIMITED BY SIZE                         
136200              '               ' DELIMITED BY SIZE                         
136300              INTO MQ-IDARTNR                                             
136400              ON OVERFLOW CONTINUE                                        
136500     END-STRING                                                           
136600                                                                          
136700     INSPECT MQ-KVANTAL REPLACING LEADING ZEROES BY SPACE                 
136800     .                                                                    
136900                                                                          
137000 S01-OUTQ-OPEN SECTION.                                                   
137100                                                                          
137200     MOVE 'OPENONL'              TO OUTQ-KDFUNC                           
137300     MOVE 'PULS.INTERFORM.LABELREQUEST'                                   
137400                                 TO OUTQ-ADDISPABS                        
137500     MOVE ZERO                   TO OUTQ-KVDLEN                           
137600     CALL WZ11OUTQ            USING OUTQ-AREA                             
137700                                                                          
137800     IF OUTQ-KDRC > 0                                                     
137900       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
138000       STRING 'WZ11OUTQ OPEN ERROR RC = ' KDRC-DISPLAY                    
138100         DELIMITED BY SIZE     INTO FELTEXT                               
138200       CALL ABEND             USING RKOD-ABEND-MED-DUMP                   
138300     END-IF                                                               
138400     .                                                                    
138500                                                                          
138600 S02-OUTQ-PUT      SECTION.                                               
138700                                                                          
138800     MOVE 'PUT'                  TO OUTQ-KDFUNC                           
138900                                                                          
139000     MOVE FUNCTION DISPLAY-OF (                                           
139100          FUNCTION NATIONAL-OF (WS-RAD, 278)                              
139200                                , 1208)                                   
139300                                 TO OUTQ-DATA                             
139400                                                                          
139500     CALL WZ11OUTQ            USING OUTQ-AREA                             
139600     IF OUTQ-KDRC > 0                                                     
139700       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
139800       STRING 'WZ11OUTQ PUT ERROR RC = ' KDRC-DISPLAY                     
139900         DELIMITED BY SIZE     INTO FELTEXT                               
140000       CALL ABEND             USING RKOD-ABEND-MED-DUMP                   
140100     END-IF                                                               
140200     .                                                                    
140300                                                                          
140400 S03-OUTQ-CLOSE SECTION.                                                  
140500                                                                          
140600     MOVE 'CLOSE'                TO OUTQ-KDFUNC                           
140700     MOVE ZERO                   TO OUTQ-KVDLEN                           
140800     CALL WZ11OUTQ            USING OUTQ-AREA                             
140900                                                                          
141000     IF OUTQ-KDRC > 0                                                     
141100       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
141200       STRING 'WZ11OUTQ CLOSE ERROR RC = ' KDRC-DISPLAY                   
141300         DELIMITED BY SIZE     INTO FELTEXT                               
141400       CALL ABEND             USING RKOD-ABEND-MED-DUMP                   
141500     END-IF                                                               
141600     .                                                                    
141700                                                                          
141800 S04-SET-PROP SECTION.                                                    
141900                                                                          
142000     MOVE ZERO                   TO OUTQ-KVDLEN                           
142100     MOVE 'SETPROP'              TO OUTQ-KDFUNC                           
142200     CALL WZ11OUTQ            USING OUTQ-AREA                             
142300                                                                          
142400     IF OUTQ-KDRC > 0                                                     
142500        MOVE OUTQ-KDRC           TO KDRC-DISPLAY                          
142600        STRING 'WZ11OUTQ INQPROP ERROR RC = ' KDRC-DISPLAY                
142700          DELIMITED BY SIZE    INTO FELTEXT                               
142800        CALL ABEND                                                        
142900     END-IF                                                               
143000     .                                                                    
143100                                                                          
143200 MFS-RENSA-FAELT-UT SECTION.                                              
143300                                                                          
143400     MOVE MFS-RENSA-FAELT TO MOD-BEARTURS                                 
143500                             MOD-KVQPACK-JUST                             
143600                             MOD-KVQPACK-0                                
143700                             MOD-KVQPACK-1                                
143800                             MOD-KVQPACK-2                                
143900                             MOD-KDSORT                                   
144000                             MOD-TEETIK-INT                               
144100                             MOD-BEART                                    
144200                             MOD-IDARTNR-ETIK                             
144300                             MOD-IDUSER                                   
144400                             MOD-TIUPPDAT                                 
144500                             MOD-IDLAYOUT                                 
144600     .                                                                    
144700     SKIP3                                                                
144800 MFS-RENSA-FAELT-IN SECTION.                                              
144900                                                                          
145000     MOVE MFS-RENSA-FAELT TO MOD-KVANTAL-ETIK-IN                          
145100                             MOD-IDPRTLST-IN                              
145200                             MOD-BEARTURS-JUST-IN                         
145300                             MOD-KVQPACK-JUST-IN                          
145400                             MOD-IDBATCH-IN                               
145500     .                                                                    
145600     EJECT                                                                
145700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
145800                                                                          
145900     MOVE MFS-ROER-EJ-FAELT TO MOD-BEARTURS                               
146000                               MOD-KVQPACK-JUST                           
146100                               MOD-KVQPACK-0                              
146200                               MOD-KVQPACK-1                              
146300                               MOD-KVQPACK-2                              
146400                               MOD-KDSORT                                 
146500                               MOD-TEETIK-INT                             
146600                               MOD-BEART                                  
146700                               MOD-IDARTNR-ETIK                           
146800                               MOD-IDUSER                                 
146900                               MOD-TIUPPDAT                               
147000                               MOD-IDLAYOUT                               
147100     .                                                                    
147200     SKIP3                                                                
147300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
147400                                                                          
147500     MOVE MFS-ROER-EJ-FAELT TO MOD-KVANTAL-ETIK-IN                        
147600                               MOD-IDPRTLST-IN                            
147700                               MOD-BEARTURS-JUST-IN                       
147800                               MOD-KVQPACK-JUST-IN                        
147900                               MOD-IDBATCH-IN                             
148000     .                                                                    
148100     EJECT                                                                
148200*MFS-FORM-ATTR SECTION.                                                   
148300*                                                                         
148400*    MOVE MFS-FORMATETS-ATTR TO MOD-KVANTAL-ETIK-ATTR                     
148500*                               MOD-IDPRTLST-ATTR                         
148600*                               MOD-BEARTURS-JUST-ATTR                    
148700*                               MOD-KVQPACK-JUST-ATTR                     
148800*                               MOD-IDBATCH-ATTR                          
148900*    .                                                                    
149000*    SKIP2                                                                
149100*MFS-LAES-IN-IGEN SECTION.                                                
149200*                                                                         
149300*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVANTAL-ETIK-ATTR                  
149400*                                  MOD-IDPRTLST-ATTR                      
149500*                                  MOD-BEARTURS-JUST-ATTR                 
149600*                                  MOD-KVQPACK-JUST-ATTR                  
149700*                                  MOD-IDBATCH-ATTR                       
149800*    .                                                                    
149900     EJECT                                                                
150000* --- IMS SEKTIONER ---                                                   
150100     SKIP3                                                                
150200 IMS-GET-MSG SECTION.                                                     
150300     MOVE '  QC' TO GODK-STATUSKODER                                      
150400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
150500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
150600     PERFORM IMS-STATUSKONTROLL                                           
150700     .                                                                    
150800     SKIP3                                                                
150900 IMS-INSERT-MSG SECTION.                                                  
151000     IF MSGI-IDLAND-SPR = 'SE'                                            
151100       MOVE '0' TO MFS-KDHUVOMR                                           
151200     END-IF                                                               
151300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
151400     MOVE SPACE TO GODK-STATUSKODER                                       
151500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
151600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
151700     PERFORM IMS-STATUSKONTROLL                                           
151800     .                                                                    
151900     EJECT                                                                
152000 IMS-GET-ARTC01 SECTION.                                                  
152100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
152200          DELIMITED BY SIZE INTO SSA1                                     
152300     MOVE '  GE' TO GODK-STATUSKODER                                      
152400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
152500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
152600     PERFORM IMS-STATUSKONTROLL                                           
152700     .                                                                    
152800     SKIP2                                                                
152900 IMS-GET-ARTC11 SECTION.                                                  
153000     MOVE 'WLARTC11 ' TO SSA1                                             
153100     MOVE '  GE' TO GODK-STATUSKODER                                      
153200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
153300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
153400     PERFORM IMS-STATUSKONTROLL                                           
153500     .                                                                    
153600     EJECT                                                                
153700 IMS-GN-INLE11 SECTION.                                                   
153800     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
153900          DELIMITED BY SIZE INTO SSA1                                     
154000     MOVE 'WLINLE11 ' TO SSA2                                             
154100     MOVE '  GE' TO GODK-STATUSKODER                                      
154200     CALL CBLTDLI USING GN INLE-PCB DLI-IO-WLINLE21 SSA1 SSA2             
154300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
154400     PERFORM IMS-STATUSKONTROLL                                           
154500     .                                                                    
154600     SKIP2                                                                
154700 IMS-GNP-INLE21 SECTION.                                                  
154800     STRING 'WLINLE21(IDPTYP   =' W-IDPTYP-X ')'                          
154900          DELIMITED BY SIZE INTO SSA1                                     
155000     MOVE '  GE' TO GODK-STATUSKODER                                      
155100     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-WLINLE21 SSA1                 
155200     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
155300     PERFORM IMS-STATUSKONTROLL                                           
155400     .                                                                    
155500     EJECT                                                                
155600 IMS-GET-BENA11 SECTION.                                                  
155700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
155800             DELIMITED BY SIZE INTO SSA1                                  
155900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
156000              DELIMITED BY SIZE INTO SSA2                                 
156100     MOVE '  GE' TO GODK-STATUSKODER                                      
156200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
156300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
156400     PERFORM IMS-STATUSKONTROLL                                           
156500     .                                                                    
156600     SKIP3                                                                
156700 IMS-GET-ETIA01 SECTION.                                                  
156800     STRING 'WLETIA01(IDARTNR  =' W-IDARTNR-X ')'                         
156900          DELIMITED BY SIZE INTO SSA1                                     
157000     MOVE '  GE' TO GODK-STATUSKODER                                      
157100     CALL CBLTDLI USING GHU ETIA-PCB DLI-IO-WLETIA01 SSA1                 
157200     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
157300     PERFORM IMS-STATUSKONTROLL                                           
157400     .                                                                    
157500     EJECT                                                                
157600 IMS-GNP-ETIA11 SECTION.                                                  
157700     MOVE 'WLETIA11 ' TO SSA1                                             
157800     MOVE '  GE' TO GODK-STATUSKODER                                      
157900     CALL CBLTDLI USING GNP ETIA-PCB DLI-IO-WLETIA11 SSA1                 
158000     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
158100     PERFORM IMS-STATUSKONTROLL                                           
158200     .                                                                    
158300     SKIP3                                                                
158400 IMS-STATUSKONTROLL SECTION.                                              
158500     SET STATUS-IX TO 1                                                   
158600     SEARCH GODK-STATUS                                                   
158700       AT END                                                             
158800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
158900         DELIMITED BY SIZE INTO FELTEXT                                   
159000         CALL FELLOG                                                      
159100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
159200         CONTINUE                                                         
159300     END-SEARCH                                                           
159400     .                                                                    
