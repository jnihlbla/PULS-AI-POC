000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4635900.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   OKTOBER 2014.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        FELMAIL DDGS-PACKTRANSAR VIA D&P                                 
000900*                                                                         
001000* INFIL W46354 - DDGS-FELPOSTER (PACKTRANASAR)                            
001100*                                                                         
001200* URFIL W4639N - DDGS-FELPOSTER TILL D&P                                  
001300*                                                                         
001410*        PROGRAMMET LÄSER      WDE4                                       
001500*                   LÄSER      WDD3                                       
001600*                   LÄSER      WDQ4                                       
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800                                                                          
002900*          --- FELTRANSAR                                                 
003000     SELECT W46354                     ASSIGN TO W46359D1.                
003100                                                                          
003200*          --- FELTRANSAR TILL D&P                                        
003300     SELECT W46354M                    ASSIGN TO W46359D2.                
003400                                                                          
003500                                                                          
003600 DATA DIVISION.                                                           
003700 FILE SECTION.                                                            
003800                                                                          
003900 FD  W46354                                                               
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W46358      -L.                                                
004400*01  -COPY W46357      -L.                                                
004500                                                                          
004600                                                                          
004700 FD  W46354M                                                              
004800     RECORDING       V                                                    
004900     BLOCK CONTAINS  0.                                                   
005000 01  DOP-POST  PIC X(100).                                                
005100                                                                          
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400 77  IDPGM                       PIC X(8)    VALUE 'W4635900'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005620                                                                          
005700 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005800 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005900 77  KDRC-DISPLAY                PIC Z(3)9.                               
006000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006100                                                                          
006200 77  W46354-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W46354                       VALUE 'J'.                   
006400                                                                          
006500                                                                          
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007010 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
007100                                                                          
007200                                                                          
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400                                                                          
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007900                                                                          
008000                                                                          
008100*    --- PARAMETRAR TILL ABEND                                            
008200                                                                          
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008600                                                                          
008700 01  HJAELP-FAELT.                                                        
008800     03  CURR-IDPRODNR           PIC 9(7)    VALUE ZERO.                  
008830     03  CURR-IDSUPREF           PIC X(10)   VALUE SPACE.                 
008831     03  CURR-DAREGDAT           PIC 9(8)    VALUE ZERO.                  
008832     03  CURR-FELTEXT            PIC X(30)   VALUE SPACE.                 
008840                                                                          
008900     03  W-IDPRODNR              PIC 9(7)    VALUE ZERO.                  
009000     03  W-KVAVBART              PIC 9(7)    VALUE ZERO.                  
009100     03  W-KVLEVART              PIC 9(7)    VALUE ZERO.                  
009200     03  W-IDKOLLI               PIC 9(5)    VALUE ZERO.                  
009210     03  W-IDRADNR               PIC 9(4)    VALUE ZERO.                  
009220     03  W-IDARTNR               PIC 9(8)    VALUE ZERO.                  
009230     03  W-KVLEVART              PIC 9(6)    VALUE ZERO.                  
009300                                                                          
009310     03  RAD-IX                  PIC 9(3)    VALUE ZERO.                  
009311     03  RAD-IX-MAX              PIC 9(3)    VALUE 74.                    
009320                                                                          
009400 01  FELTEXT.                                                             
009500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009700                                                                          
009800                                                                          
009900 01  MAIL-AREA.                                                           
010100     03  MAIL-BLANKRAD           PIC X       VALUE SPACE.                 
010300                                                                          
010310     03  MAIL-INTRO-1.                                                    
010320         05  FILLER              PIC X(05)   VALUE                        
010330            'Hello'.                                                      
010340     03  MAIL-INTRO-2.                                                    
010350         05  FILLER              PIC X(43)   VALUE                        
010360            'BP2T7 has received an EDI transaction from '.                
010361         05  INTRO-IDLEVNR       PIC X(05)   VALUE SPACE.                 
010362         05  FILLER              PIC X(30)   VALUE                        
010363            ' that is declined in the PULS.'.                             
010370     03  MAIL-INTRO-3.                                                    
010371         05  FILLER              PIC X(25)   VALUE                        
010372            'The message was received '.                                  
010373         05  INTRO-DATE          PIC X(06)   VALUE SPACE.                 
010374         05  FILLER              PIC X(01)   VALUE SPACE.                 
010376         05  INTRO-TIME          PIC X(05)   VALUE SPACE.                 
010391     03  MAIL-INTRO-4.                                                    
010392         05  FILLER              PIC X(13)   VALUE                        
010393            'Message type:'.                                              
010400     03  MAIL-RUBRIK.                                                     
010500         05  FILLER              PIC X(25)   VALUE                        
010600            ' Rejected Despatch Advice'.                                  
010700         05  FILLER              PIC X(20)   VALUE SPACE.                 
010800         05  FILLER              PIC X(19)   VALUE                        
010900            'Productionnumber.: '.                                        
011000         05  MAIL-IDPRODNR       PIC Z(6)9   VALUE ZERO.                  
011100                                                                          
011200     03  MAIL-REFERENS.                                                   
011300         05  FILLER              PIC X(19)   VALUE                        
011400            ' Referece........: '.                                        
011500         05  MAIL-IDSUPREF       PIC X(10)   VALUE SPACE.                 
011600                                                                          
011700     03  MAIL-DATUM.                                                      
011800         05  FILLER              PIC X(19)   VALUE                        
011900            ' Date............: '.                                        
012000         05  MAIL-DAREGDAT       PIC 9(8)    VALUE ZERO.                  
012100                                                                          
012200     03  MAIL-FELORSAK.                                                   
012300         05  FILLER              PIC X(19)   VALUE                        
012400            ' Error Reasoncode: '.                                        
012500         05  MAIL-FELTEXT        PIC X(30)   VALUE SPACE.                 
012600                                                                          
012700                                                                          
012800                                                                          
012900     03  MAIL-ORDER.                                                      
013000         05  FILLER              PIC X(11)   VALUE                        
013100            ' Supplier: '.                                                
013200         05  MAIL-IDLEVNR        PIC X(5)    VALUE SPACE.                 
013300         05  FILLER              PIC X(12)   VALUE                        
013400            '  District: '.                                               
013500         05  MAIL-IDDISTR        PIC Z(3)9   VALUE ZERO.                  
013600         05  FILLER              PIC X(8)    VALUE                        
013700            '  Cust: '.                                                   
013800         05  MAIL-IDKUNDNR       PIC Z(5)9   VALUE ZERO.                  
013900         05  FILLER              PIC X(12)   VALUE                        
014000            '  Order no: '.                                               
014100         05  MAIL-IDORDNR        PIC Z(4)9   VALUE ZERO.                  
014200         05  FILLER              PIC X(6)    VALUE                        
014300            '  DC: '.                                                     
014400         05  MAIL-IDDC           PIC X(2)    VALUE SPACE.                 
014500                                                                          
014600                                                                          
014700                                                                          
014800     03  MAIL-KOLLI.                                                      
014900         05  FILLER              PIC X(7)    VALUE                        
015000            ' Case: '.                                                    
015100         05  MAIL-IDKOLLI        PIC X(5)    VALUE SPACE.                 
015200                                                                          
015300                                                                          
015400                                                                          
015500     03  MAIL-RADRUBRIK.                                                  
015600         05  FILLER              PIC X(25)   VALUE                        
015700            ' Line no  Part        Qty'.                                  
015800                                                                          
015900     03  MAIL-RAD.                                                        
015910         05  FILLER              PIC X(1)    VALUE SPACE.                 
016000         05  MAIL-IDRADNR        PIC Z(4)    VALUE ZERO.                  
016100         05  FILLER              PIC X(5)    VALUE SPACE.                 
016200         05  MAIL-IDARTNR        PIC Z(8)    VALUE ZERO.                  
016300         05  FILLER              PIC X(1)    VALUE SPACE.                 
016400         05  MAIL-KVLEVART       PIC Z(6)    VALUE ZERO.                  
016500                                                                          
016600                                                                          
016700                                                                          
016800     03  MAIL-PULS-RUBRIK.                                                
016900         05  FILLER              PIC X(37)   VALUE                        
017000            ' This is the order in the PULS system'.                      
017100                                                                          
017200                                                                          
017300                                                                          
017400     03  MAIL-SEPARATOR.                                                  
017500         05  FILLER              PIC X(40)   VALUE                        
017600            '----------------------------------------'.                   
017601         05  FILLER              PIC X(40)   VALUE                        
017602            '----------------------------------------'.                   
017610                                                                          
017620     03  MAIL-PULS-ORDER.                                                 
017630         05  FILLER              PIC X(11)   VALUE                        
017640            ' District: '.                                                
017700         05  MAIL-IDDISTR-PULS   PIC Z(3)9   VALUE ZERO.                  
017800         05  FILLER              PIC X(8)    VALUE                        
017900            '  Cust: '.                                                   
018000         05  MAIL-IDKUNDNR-PULS  PIC Z(5)9   VALUE ZERO.                  
018100         05  FILLER              PIC X(15)   VALUE                        
018200            '  Order no: '.                                               
018300         05  MAIL-IDORDNR-PULS   PIC Z(4)9   VALUE ZERO.                  
018400         05  FILLER              PIC X(6)    VALUE                        
018500            '  DC: '.                                                     
018600         05  MAIL-IDDC-PULS      PIC X(2)    VALUE SPACE.                 
018700                                                                          
018800                                                                          
018900                                                                          
019000     03  MAIL-PULS-RADRUBRIK.                                             
019100         05  FILLER              PIC X(31)   VALUE                        
019200            ' Line no  Part      Description'.                            
019300         05  FILLER              PIC X(18)   VALUE SPACE.                 
019400         05  FILLER              PIC X(26)   VALUE                        
019500            'Ordered   Delivered   Case'.                                 
019600                                                                          
019700                                                                          
019800                                                                          
019900     03  MAIL-PULS-RAD.                                                   
020000         05  FILLER              PIC X(1)    VALUE SPACE.                 
020100         05  MAIL-IDRADNR-PULS   PIC Z(3)9   VALUE ZERO.                  
020200         05  FILLER              PIC X(5)    VALUE SPACE.                 
020300         05  MAIL-IDARTNR-PULS   PIC Z(7)9   VALUE ZERO.                  
020400         05  FILLER              PIC X(2)    VALUE SPACE.                 
020500         05  MAIL-BEART-PULS     PIC X(25)   VALUE SPACE.                 
020600         05  FILLER              PIC X(3)    VALUE SPACE.                 
020700         05  MAIL-KVAVBART-PULS  PIC Z(6)9   VALUE ZERO.                  
020800         05  FILLER              PIC X(3)    VALUE SPACE.                 
020900         05  MAIL-KVLEVART-PULS  PIC Z(6)9   VALUE ZERO.                  
021000         05  FILLER              PIC X(5)    VALUE SPACE.                 
021100         05  MAIL-IDKOLLI-PULS   PIC Z(5)    VALUE ZERO.                  
021110                                                                          
021120     03  MAIL-END-1.                                                      
021130         05  FILLER              PIC X(51)   VALUE                        
021140           'If the EDI-message included several order or lines,'.         
021150         05  FILLER              PIC X(28)   VALUE                        
021160           ' none of those was accepted.'.                                
021170     03  MAIL-END-2.                                                      
021180         05  FILLER              PIC X(46)   VALUE                        
021190           'Please check your message, correct and resend.'.              
021191     03  MAIL-END-3.                                                      
021192         05  FILLER              PIC X(46)   VALUE                        
021193           'If you have any questions about this message, '.              
021194         05  FILLER              PIC X(21)   VALUE                        
021195           'please send a mail to'.                                       
021196     03  MAIL-END-4.                                                      
021197         05  FILLER              PIC X(22)   VALUE                        
021198           'DCIDHELP@VOLVOCARS.COM'.                                      
021200                                                                          
021210 01  W001-DAP.                                                            
021220     03  FILLER                  PIC X(165)  VALUE SPACE.                 
021300*    --- PARAMETRAR TILL POSTSUM                                          
021400                                                                          
021500*01  -COPY W0005   -PRE  POSTSUM-                                         
021600                                                                          
021700                                                                          
021800 01  W46354-AREA-START           PIC X(24)   VALUE                        
021900                                 'W46354-AREA-START  '.                   
022000                                                                          
022100 01  W46354-AREA.                                                         
022200     03  W46354-AREA-0.                                                   
022300       05  FILLER                PIC X(2000).                             
022400*   03  FILLER -COPY W46358  -PRE FEL-  -RED  W46354-AREA-0               
022500*   03  FILLER -COPY W46357  -PRE RAD-  -RED  W46354-AREA-0               
022600                                                                          
022700                                                                          
022800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022900                                                                          
023000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023100                                                                          
023200 01  NYCKLAR-TILL-DLI.                                                    
023222     03  W-WDE4E1KY-MAX-X.                                                
023223         05 W-IDPRODNR-WDE4E-MAX  PIC S9(7)   VALUE ZERO  COMP-3.         
023224         05 W-WDE4E1-MAX          PIC X(19)   VALUE HIGH-VALUE.           
023225                                                                          
023229     03  W-WDE4E1KY-MIN-X.                                                
023230         05 W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.         
023231         05 W-WDE4E1-MIN          PIC X(19)   VALUE LOW-VALUE.            
023240                                                                          
023300     03  W-WDE401KY-X.                                                    
023400         05 W-IDDISTR-E4          PIC S9(5)   VALUE ZERO  COMP-3.         
023500         05 W-IDKUNDNR-E4         PIC S9(7)   VALUE ZERO  COMP-3.         
023600         05 W-IDKUNDRF-E4         PIC X(10)   VALUE SPACE.                
023700         05 W-IDPRODNR-E4         PIC S9(7)   VALUE ZERO  COMP-3.         
023800         05 W-IDPLKLST-E4         PIC S9(3)   VALUE ZERO  COMP-3.         
023900                                                                          
023910     03  W-IDPRODNR-E6-X.                                                 
023920         05  W-IDPRODNR-E6       PIC S9(7)   VALUE ZERO COMP-3.           
023930                                                                          
024000     03  W-IDPURAD-X.                                                     
024100         05  W-IDPURAD            PIC S9(5)   VALUE ZERO COMP-3.          
024200                                                                          
024300     03  W-IDARTNR-X.                                                     
024400         05 W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.         
024500                                                                          
024600     03  W-IDSKYLT-X.                                                     
024700         05 W-IDSKYLT             PIC X(3)    VALUE 'GB'.                 
024800                                                                          
024900     03  W-WDQ401KY-MIN-X.                                                
025000         05  W-IDORDER-MIN        PIC S9(7)   VALUE ZERO COMP-3.          
025100         05  W-IDDC-MIN           PIC X(2)    VALUE SPACE.                
025200         05  FILLER               PIC X(14)   VALUE SPACE.                
025300                                                                          
025400     03  W-WDQ401KY-MAX-X.                                                
025500         05  W-IDORDER-MAX        PIC S9(7)   VALUE ZERO COMP-3.          
025600         05  W-IDDC-MAX           PIC X(2)    VALUE SPACE.                
025700         05  FILLER               PIC X(14)   VALUE SPACE.                
025800                                                                          
025900*    --- STATUS-KOD FRÅN IMS                                              
026000 01  STATUS-WS                   PIC XX.                                  
026100     88  SEGMENT-FINNS                       VALUE '  '.                  
026200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026400                                                                          
026500 01  GODK-STATUSKODER.                                                    
026600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026700                                                                          
026800 01  ALL-SSA.                                                             
026900     03 SSA1                     PIC X(128).                              
027000     03 SSA2                     PIC X(128).                              
027100                                                                          
027200                                                                          
027300*    --- IMS FUNKTIONSKODER                                               
027400*01  -COPY W0003                                                          
027500                                                                          
027600*    ---  DLI INPUT-OUTPUT AREA                                           
027700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE4E1'.                      
027800 01  DLI-IO-WDE4E1.                                                       
027900*    03  -COPY WDE4E1                                                     
027910                                                                          
027920*    ---  DLI INPUT-OUTPUT AREA                                           
027930 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
027940 01  DLI-IO-WDE401.                                                       
027950*    03  -COPY WDE401                                                     
028000                                                                          
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE411'.                      
028200 01  DLI-IO-WDE411.                                                       
028300*    03  -COPY WDE411  -PRE E4-                                           
028400                                                                          
028500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE421'.                      
028600 01  DLI-IO-WDE421.                                                       
028700*    03  -COPY WDE421                                                     
028800                                                                          
028900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
029000 01  DLI-IO-WDE601.                                                       
029100*    03  -COPY WDE601                                                     
029110                                                                          
029120 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
029130 01  DLI-IO-WDD311.                                                       
029140*    03  -COPY WDD311                                                     
029200                                                                          
029300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ401'.                      
029400 01  DLI-IO-WDQ401.                                                       
029500*    03  -COPY WDQ401                                                     
029600                                                                          
029700                                                                          
029800 LINKAGE SECTION.                                                         
029900                                                                          
030000                                                                          
030310*01  -COPY W0008  -PRE WDE4E-                                             
030320     05  FILLER                  PIC X.                                   
030330                                                                          
030340*01  -COPY W0008  -PRE WDE4-                                              
030350     05  FILLER                  PIC X.                                   
030360                                                                          
030400*01  -COPY W0008  -PRE WDE6-                                              
030500     05  FILLER                  PIC X.                                   
030600                                                                          
030610*01  -COPY W0008  -PRE WDD3-                                              
030620     05  FILLER                  PIC X.                                   
030630                                                                          
030700*01  -COPY W0008  -PRE WDQ4-                                              
030800     05  FILLER                  PIC X.                                   
030900                                                                          
031000                                                                          
031100 PROCEDURE DIVISION  USING WDE4E-PCB WDE4-PCB WDE6-PCB                    
031110                           WDD3-PCB WDQ4-PCB.                             
031200 MAIN SECTION.                                                            
031300     ENTRY 'DLITCBL' USING WDE4E-PCB WDE4-PCB WDE6-PCB                    
031400                           WDD3-PCB WDQ4-PCB.                             
031500                                                                          
031600     PERFORM A-INIT                                                       
031700     PERFORM S01-LAES-W46354                                              
031800                                                                          
031900     PERFORM UNTIL END-OF-W46354                                          
032000                                                                          
032300        PERFORM B-NYTT-IDPRODNR                                           
032400        PERFORM S01-LAES-W46354                                           
032410        IF NOT END-OF-W46354                                              
032500           PERFORM C-SKRIV-HEADER                                         
032600           PERFORM D-NY-ORDER                                             
033100                                                                          
033101           PERFORM UNTIL END-OF-W46354                                    
033102           OR RAD-IDPRODNR NOT = CURR-IDPRODNR                            
033110              PERFORM E-NYTT-KOLLI                                        
033111              PERFORM S01-LAES-W46354                                     
033120           END-PERFORM                                                    
033130                                                                          
033140           PERFORM F-VISA-PULSORDER                                       
033150        END-IF                                                            
033200                                                                          
033400     END-PERFORM                                                          
033500                                                                          
033600                                                                          
033800     PERFORM Z-FINIT                                                      
033900                                                                          
034000     MOVE ZERO TO RETURN-CODE                                             
034100     GOBACK                                                               
034200     .                                                                    
034300                                                                          
034400                                                                          
034500 A-INIT SECTION.                                                          
034600     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
034700                                                                          
034800     OPEN INPUT  W46354                                                   
034810     OPEN OUTPUT W46354M                                                  
034900                                                                          
035000     ACCEPT DAGENS-DATUM  FROM DATE                                       
035010     ACCEPT DAGENS-TID    FROM TIME                                       
035100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
035700     .                                                                    
035800                                                                          
035900                                                                          
036000 B-NYTT-IDPRODNR SECTION.                                                 
036100     MOVE 'B-IDPRODNR      ' TO CURRENT-SECTION                           
036200                                                                          
036313     MOVE FEL-IDPRODNR     TO W-IDPRODNR                                  
036320                              CURR-IDPRODNR                               
036330                                                                          
036800     MOVE FEL-IDSUPREF     TO CURR-IDSUPREF                               
037600     MOVE FEL-FELTEXT      TO CURR-FELTEXT                                
037700     MOVE FEL-DAREGDAT     TO CURR-DAREGDAT                               
038300                                                                          
039400     .                                                                    
039500                                                                          
039600                                                                          
039601 C-SKRIV-HEADER  SECTION.                                                 
039602     MOVE 'C-SKRIV-HEADER  ' TO CURRENT-SECTION                           
039603                                                                          
039604     MOVE '¤DAPDDGSPTFEL'  TO W001-DAP                                    
039605     WRITE DOP-POST FROM W001-DAP                                         
039606                                                                          
039607     MOVE SPACE            TO W001-DAP                                    
039609     STRING '¤DAP' RAD-IDANSTNR                                           
039610     DELIMITED BY SIZE   INTO W001-DAP                                    
039615     WRITE DOP-POST FROM W001-DAP                                         
039616                                                                          
039617     MOVE RAD-IDANSTNR     TO INTRO-IDLEVNR                               
039618     WRITE DOP-POST FROM MAIL-INTRO-1                                     
039619     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
039620     WRITE DOP-POST FROM MAIL-INTRO-2                                     
039621     MOVE DAGENS-DATUM     TO INTRO-DATE                                  
039622     STRING DAGENS-TID(1:2) ':' DAGENS-TID(3:2)                           
039623            DELIMITED BY SIZE                                             
039624                          INTO INTRO-TIME                                 
039625     WRITE DOP-POST FROM MAIL-INTRO-3                                     
039626     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
039627     WRITE DOP-POST FROM MAIL-INTRO-4                                     
039628                                                                          
039629     MOVE CURR-IDPRODNR    TO MAIL-IDPRODNR                               
039630     WRITE DOP-POST FROM MAIL-RUBRIK                                      
039631                                                                          
039632     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
039633     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
039634                                                                          
039635     MOVE CURR-IDSUPREF    TO MAIL-IDSUPREF                               
039636     WRITE DOP-POST FROM MAIL-REFERENS                                    
039637                                                                          
039638     MOVE CURR-DAREGDAT    TO MAIL-DAREGDAT                               
039639     WRITE DOP-POST FROM MAIL-DATUM                                       
039640                                                                          
039641     MOVE CURR-FELTEXT     TO MAIL-FELTEXT                                
039642     WRITE DOP-POST FROM MAIL-FELORSAK                                    
039643                                                                          
039644     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
039645                                                                          
039646     .                                                                    
039647                                                                          
039648                                                                          
039649 D-NY-ORDER SECTION.                                                      
039650     MOVE 'C-ORDER         ' TO CURRENT-SECTION                           
039660                                                                          
039705                                                                          
039707     MOVE RAD-IDANSTNR     TO MAIL-IDLEVNR                                
039708     IF RAD-IDDISTR = ALL '+'                                             
039709        MOVE ZERO          TO MAIL-IDDISTR                                
039710     ELSE                                                                 
039711        MOVE RAD-IDDISTR   TO MAIL-IDDISTR                                
039712     END-IF                                                               
039713     MOVE RAD-IDKUNDNR     TO MAIL-IDKUNDNR                               
039714     MOVE RAD-IDORDNR      TO MAIL-IDORDNR                                
039715     IF RAD-IDDC = ALL '+'                                                
039716        MOVE SPACE         TO MAIL-IDDC                                   
039717     ELSE                                                                 
039718       MOVE RAD-IDDC       TO MAIL-IDDC                                   
039719     END-IF                                                               
039720     WRITE DOP-POST FROM MAIL-ORDER                                       
039721                                                                          
039722     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
039723     .                                                                    
039724                                                                          
039725                                                                          
039726 E-NYTT-KOLLI SECTION.                                                    
039727     MOVE 'E-NYTT-KOLLI    ' TO CURRENT-SECTION                           
039728                                                                          
039729                                                                          
039730     IF RAD-IDDISTR = ALL '+'                                             
039731        MOVE SPACE         TO MAIL-IDKOLLI                                
039732     ELSE                                                                 
039733        MOVE RAD-IDKOLLI   TO MAIL-IDKOLLI                                
039734     END-IF                                                               
039735     WRITE DOP-POST FROM MAIL-KOLLI                                       
039736     WRITE DOP-POST FROM MAIL-RADRUBRIK                                   
039737                                                                          
039738     MOVE 1 TO RAD-IX                                                     
039739     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
039740                OR RAD-IDRADNR(RAD-IX) = ALL '+'                          
039741        MOVE SPACE                 TO MAIL-RAD                            
039742        MOVE RAD-IDRADNR(RAD-IX)   TO MAIL-IDRADNR                        
039743        MOVE RAD-IDARTNR(RAD-IX)   TO MAIL-IDARTNR                        
039744        MOVE RAD-KVLEVART(RAD-IX)  TO MAIL-KVLEVART                       
039745        WRITE DOP-POST FROM MAIL-RAD                                      
039746        ADD 1 TO RAD-IX                                                   
039747     END-PERFORM                                                          
039748                                                                          
039749     WRITE DOP-POST FROM MAIL-BLANKRAD                                    
039750     .                                                                    
039751                                                                          
039752                                                                          
039760 F-VISA-PULSORDER SECTION.                                                
039800     MOVE 'F-VISA-PULSORDER' TO CURRENT-SECTION                           
039900                                                                          
039902     MOVE CURR-IDPRODNR      TO W-IDPRODNR-E6                             
039903                                W-IDPRODNR-WDE4E-MAX                      
039904                                W-IDPRODNR-WDE4E-MIN                      
039905     PERFORM IMS-GU-WDE601                                                
039906                                                                          
039907     IF SEGMENT-FINNS                                                     
039908                                                                          
039909        WRITE DOP-POST FROM MAIL-SEPARATOR                                
039910        WRITE DOP-POST FROM MAIL-BLANKRAD                                 
039911        WRITE DOP-POST FROM MAIL-PULS-RUBRIK                              
039912        WRITE DOP-POST FROM MAIL-BLANKRAD                                 
039913                                                                          
039914        PERFORM IMS-GU-WDE4E1                                             
039915        MOVE SEQE-IDDISTR TO W-IDDISTR-E4                                 
039916        MOVE SEQE-IDKUNDNR TO W-IDKUNDNR-E4                               
039917        MOVE SEQE-IDKUNDRF TO W-IDKUNDRF-E4                               
039918        MOVE SEQE-IDPRODNR TO W-IDPRODNR-E4                               
039919        MOVE SEQE-IDPLKLST TO W-IDPLKLST-E4                               
039920        PERFORM IMS-GU-WDE401                                             
039921                                                                          
039922        MOVE KORD-IDDISTR    TO MAIL-IDDISTR-PULS                         
039923        MOVE KORD-IDKUNDNR   TO MAIL-IDKUNDNR-PULS                        
039924        MOVE KORD-IDORDNR5   TO MAIL-IDORDNR-PULS                         
039925        MOVE KORD-IDDC       TO MAIL-IDDC-PULS                            
039926        WRITE DOP-POST FROM MAIL-PULS-ORDER                               
039927        WRITE DOP-POST FROM MAIL-BLANKRAD                                 
039928                                                                          
039929        PERFORM EA-VISA-PULSRADER                                         
039930        WRITE DOP-POST FROM MAIL-BLANKRAD                                 
039931        WRITE DOP-POST FROM MAIL-END-1                                    
039932        WRITE DOP-POST FROM MAIL-END-2                                    
039933        WRITE DOP-POST FROM MAIL-BLANKRAD                                 
039934        WRITE DOP-POST FROM MAIL-END-3                                    
039935        WRITE DOP-POST FROM MAIL-END-4                                    
039936     END-IF                                                               
039937                                                                          
039938     .                                                                    
039939                                                                          
039940                                                                          
039941 EA-VISA-PULSRADER SECTION.                                               
039942     MOVE 'EA-VISA-PULSRAD ' TO CURRENT-SECTION                           
039943                                                                          
039944     WRITE DOP-POST FROM MAIL-PULS-RADRUBRIK                              
039945                                                                          
039946     PERFORM IMS-GNP-WDE411                                               
039947                                                                          
039948     PERFORM UNTIL SEGMENT-SAKNAS                                         
039949                                                                          
039950        MOVE E4-ORAD-IDPURAD   TO MAIL-IDRADNR-PULS                       
039951                                  W-IDPURAD                               
039952        MOVE E4-ORAD-IDARTNR   TO MAIL-IDARTNR-PULS                       
039953        MOVE E4-ORAD-BEART     TO MAIL-BEART-PULS                         
039954        MOVE E4-ORAD-KVAVBART  TO MAIL-KVAVBART-PULS                      
039955        MOVE E4-ORAD-KVLEVART  TO MAIL-KVLEVART-PULS                      
039956        PERFORM IMS-GNP-WDE421                                            
039958        IF SEGMENT-FINNS                                                  
039959           MOVE KKOLLI-IDKOLLI TO MAIL-IDKOLLI-PULS                       
039960        ELSE                                                              
039961           MOVE ZERO           TO MAIL-IDKOLLI-PULS                       
039962        END-IF                                                            
039963                                                                          
039964        WRITE DOP-POST FROM MAIL-PULS-RAD                                 
039965                                                                          
039967        PERFORM IMS-GNP-WDE411                                            
039969     END-PERFORM                                                          
039970                                                                          
039971     .                                                                    
039972                                                                          
039973                                                                          
039974 Z-FINIT SECTION.                                                         
039975     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
039980                                                                          
040000     CLOSE W46354 W46354M                                                 
040100                                                                          
040200     MOVE 'S' TO POSTSUM-OPKOD                                            
040300     CALL POSTSUM USING POSTSUM-PARM                                      
040400     .                                                                    
040500                                                                          
040600                                                                          
040700 S01-LAES-W46354  SECTION.                                                
040800                                                                          
040900     READ W46354 INTO W46354-AREA                                         
041000     AT END                                                               
041100        MOVE HIGH-VALUE TO W46354-AREA                                    
041200        SET END-OF-W46354 TO TRUE                                         
041300                                                                          
041400     NOT AT END                                                           
041500        MOVE 'W46354'   TO POSTSUM-FDNAMN                                 
041600        MOVE 'W46359D1' TO POSTSUM-DDNAMN2                                
041700        MOVE SPACE      TO POSTSUM-TRANSTYP                               
041800        CALL POSTSUM USING POSTSUM-PARM                                   
041900     END-READ                                                             
042000     .                                                                    
042100                                                                          
042200                                                                          
046600 S99-ABEND SECTION.                                                       
046700                                                                          
046800     SKIP2                                                                
046900     MOVE 'S'        TO POSTSUM-OPKOD                                     
047000     CALL POSTSUM USING POSTSUM-PARM                                      
047100     CALL ABEND   USING RKOD-ABEND                                        
047200     .                                                                    
047300                                                                          
047400                                                                          
047500* --- IMS SEKTIONER ---                                                   
047600                                                                          
047700     EJECT                                                                
047710 IMS-GU-WDE601 SECTION.                                                   
047711     MOVE 'IMS-GU-WDE601   ' TO CURRENT-IMS-SECTION                       
047712                                                                          
047713     MOVE SPACE            TO ALL-SSA                                     
047720                                                                          
047730     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
047740          DELIMITED BY SIZE INTO SSA1                                     
047750     MOVE '  GE' TO GODK-STATUSKODER                                      
047760     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
047770     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
047780     PERFORM IMS-STATUSKONTROLL                                           
047790     .                                                                    
047791                                                                          
047792 IMS-GU-WDE4E1 SECTION.                                                   
047793     MOVE 'IMS-GU-WDE4E1   ' TO CURRENT-IMS-SECTION                       
047794                                                                          
047795     MOVE SPACE            TO ALL-SSA                                     
047796                                                                          
047797     STRING 'WDE4E1  (WDE4E1KY=>' W-WDE4E1KY-MIN-X ')'                    
047798                    '&WDE4E1KY=<' W-WDE4E1KY-MAX-X ')'                    
047799            DELIMITED BY SIZE INTO SSA1                                   
047800     MOVE '    ' TO GODK-STATUSKODER                                      
047801     CALL CBLTDLI USING GU WDE4E-PCB DLI-IO-WDE4E1 SSA1                   
047802     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
047803     PERFORM IMS-STATUSKONTROLL                                           
047804     EJECT                                                                
047805     .                                                                    
047806                                                                          
047810 IMS-GU-WDE401 SECTION.                                                   
047900     MOVE 'IMS-GU-WDE401   ' TO CURRENT-IMS-SECTION                       
048000                                                                          
048100     MOVE SPACE            TO ALL-SSA                                     
048200                                                                          
048300     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
048400       DELIMITED BY SIZE INTO SSA1                                        
048500     MOVE '  GE'           TO GODK-STATUSKODER                            
048600     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
048700     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
048800     PERFORM IMS-STATUSKONTROLL                                           
048900     .                                                                    
049000                                                                          
049100 IMS-GNP-WDE411 SECTION.                                                  
049200     MOVE 'IMS-GNP-WDE411  ' TO CURRENT-IMS-SECTION                       
049300                                                                          
049400     MOVE SPACE            TO ALL-SSA                                     
049500                                                                          
049600     MOVE 'WDE411 '        TO SSA1                                        
049700     MOVE '  GE'           TO GODK-STATUSKODER                            
049800     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE411 SSA1                   
049900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
050000     PERFORM IMS-STATUSKONTROLL                                           
050100     .                                                                    
050200                                                                          
050300 IMS-GNP-WDE421 SECTION.                                                  
050400     MOVE 'IMS-GNP-WDE421  ' TO CURRENT-IMS-SECTION                       
050500                                                                          
050600     MOVE SPACE            TO ALL-SSA                                     
050700                                                                          
050800     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
050900       DELIMITED BY SIZE INTO SSA1                                        
051000     MOVE 'WDE421 '        TO SSA2                                        
051100     MOVE '  GE'           TO GODK-STATUSKODER                            
051200     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE421 SSA1 SSA2              
051300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
051400     PERFORM IMS-STATUSKONTROLL                                           
051500     .                                                                    
051600                                                                          
051700                                                                          
051800 IMS-GU-WDD311 SECTION.                                                   
051900     MOVE 'IMS-GU-WDD311   ' TO CURRENT-IMS-SECTION                       
052000                                                                          
052100     MOVE SPACE            TO ALL-SSA                                     
052200                                                                          
052300     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
052400       DELIMITED BY SIZE INTO SSA1                                        
052500     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
052600       DELIMITED BY SIZE INTO SSA2                                        
052700     MOVE '  GE'           TO GODK-STATUSKODER                            
052800     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
052900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
053000     PERFORM IMS-STATUSKONTROLL                                           
053100     .                                                                    
053200                                                                          
053300                                                                          
053400 IMS-GU-WDQ401 SECTION.                                                   
053500     MOVE 'IMS-GU-WDQ401   ' TO CURRENT-IMS-SECTION                       
053600                                                                          
053700     MOVE SPACE            TO ALL-SSA                                     
053800                                                                          
053900     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
054000                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
054100       DELIMITED BY SIZE INTO SSA1                                        
054200     MOVE '  GE'           TO GODK-STATUSKODER                            
054300     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-WDQ401 SSA1                    
054400     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
054500     PERFORM IMS-STATUSKONTROLL                                           
054600     .                                                                    
054700                                                                          
054800 IMS-GN-WDQ401 SECTION.                                                   
054900     MOVE 'IMS-GN-WDQ401   ' TO CURRENT-IMS-SECTION                       
055000                                                                          
055100     MOVE SPACE            TO ALL-SSA                                     
055200                                                                          
055300     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
055400                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
055500       DELIMITED BY SIZE INTO SSA1                                        
055600     MOVE '  GBGE'         TO GODK-STATUSKODER                            
055700     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-WDQ401 SSA1                    
055800     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
055900     PERFORM IMS-STATUSKONTROLL                                           
056000     .                                                                    
056100                                                                          
056200                                                                          
056300 IMS-STATUSKONTROLL SECTION.                                              
056400                                                                          
056500     SET STATUS-IX TO 1                                                   
056600     SEARCH GODK-STATUS                                                   
056700       AT END                                                             
056800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
056900           DELIMITED BY SIZE INTO FELTEXT                                 
057000         DISPLAY FELTEXT                                                  
057100         CALL FELLOG                                                      
057200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
057300         CONTINUE                                                         
057400     END-SEARCH                                                           
057500     .                                                                    
