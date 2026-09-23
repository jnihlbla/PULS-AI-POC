000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WL011600.                                                
000400 AUTHOR.         M LUNDBERG.                                              
000500 DATE-WRITTEN.   JAN  1986.                                               
000600*AUTHOR.         BERT ANDERSSON.                                          
000700*DATE-WRITTEN.   MAJ   2005.                                              
000800                                                                          
000900     REMARKS.                                                             
001000* WL011600 PROGRAM IS A REPLICA OF W4032100 PROGRAM                       
001100* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001200*                                                                         
001300*    NAMN:       CARPARTS.LDC.UNREPORTEDLINES                             
001400*                                                                         
001500*                                                                         
001600*    FUNKTION.                                                            
001700*        EJ RAPP RADER PER ORDER.                                         
001800*        NOT REPORTED LINES PER ORDER                                     
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: WL0116U                                             
002200*        REQUEST:     WL0116I1                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        RESPONSE:    WL0116O1                                            
002600*    SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP3                                                                
002900 DATA DIVISION.                                                           
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77   PROGRAM-NAMN           VALUE 'WL011600'                             
003500                                 PIC X(8).                                
003600 77    JA                        PIC X       VALUE 'J'.                   
003700 77    NEJ                       PIC X       VALUE 'N'.                   
003800 77    BORTTAG                   PIC X       VALUE 'B'.                   
003900 77    SPRAK-INDX                PIC S9(9)   VALUE +0   COMP-3.           
004000 77    WS-RAD-IND                PIC S9(9)   VALUE +0   COMP-3.           
004100 77    WS-RESP-MAX-500           PIC S9(9)   VALUE +500 COMP-3.           
004110 77    WS-KVRADER                PIC  9(9).                               
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77    FILLER                    PIC X(08) VALUE 'AAAAAAAA'.              
004400 77    FEL-TEXT                  PIC X(32) VALUE SPACE.                   
004500 77    PGM-POS                   PIC X(32) VALUE SPACE.                   
004600 77    KDRC-DISPLAY              PIC Z(5).                                
004700 77    KDRC-DISP                 PIC 9(4)   VALUE ZERO.                   
004800                                                                          
004900 77    WS-SPAR-KDORDSTA          PIC S9(1)              COMP-3.           
005000 77    WS-JFR-IDPRODNR           PIC S9(7)              COMP-3.           
005100 77    WS-IDKUNDRF               PIC X(10).                               
005200 77    WS-KVORAPP                PIC S9(7) VALUE ZERO COMP-3.             
005300 77    WS-SPAR-IDPURAD           PIC S9(5) VALUE ZERO COMP-3.             
005400 77    WS-START-IDPURAD          PIC S9(5) VALUE ZERO COMP-3.             
005500 77    WS-STOPP-IDPURAD          PIC S9(5) VALUE ZERO COMP-3.             
005600 77    WS-SPAR-KVORDRAD-LEVPLC1  PIC S9(5) VALUE ZERO COMP-3.             
005700 77    WS-SPAR-KVORDRAD-LEVPLC2  PIC S9(5) VALUE ZERO COMP-3.             
005800 77    FILLER                    PIC X(08) VALUE 'BBBBBBBB'.              
005900 01  W-SPAR-IDKUNDRF.                                                     
006000     03  FILLER                  PIC X(2)    VALUE '00'.                  
006100     03  W-SPAR-IDORDNR5         PIC X(5)    VALUE '+++++'.               
006200     03  FILLER                  PIC X(3)    VALUE '+++'.                 
006300                                                                          
006400*77    WS-SPAR-KVORDRAD-LEVPL    PIC S9(5) VALUE ZERO COMP-3.             
006500*LINE DELETED                                                             
006600     SKIP2                                                                
006700*      - - - - - - - - - - - - - *****                                    
006800 77    WS-KDMFSFOR               PIC 9       VALUE ZERO.                  
006900 77    WS-KDFEL                  PIC 9(2)    VALUE ZERO COMP-3.           
007000     SKIP2                                                                
007010 77    FILLER                    PIC X(08) VALUE 'TESTTEST'.              
007100 01    WS-IDPRODNR-TEST                   PIC 9(7).                       
007101*                                                                         
007110 01    WS-IDPRODNR                        PIC X(7).                       
007200 01    IDPRODNR-WS REDEFINES WS-IDPRODNR  PIC 9(7).                       
007210 01    WS-IDPRODNR-XTRA                   PIC X(7).                       
007300     SKIP2                                                                
007400 01    WS-IDDISTR                         PIC X(4).                       
007500 01    IDDISTR-WS REDEFINES WS-IDDISTR    PIC 9(4).                       
007600     SKIP2                                                                
007700 01    WS-IDKUNDNR                        PIC X(6).                       
007800 01    IDKUNDNR-WS REDEFINES WS-IDKUNDNR  PIC 9(6).                       
007900     SKIP2                                                                
008000 01    WS-IDORDNR                         PIC X(5).                       
008100 01    IDORDNR-WS REDEFINES WS-IDORDNR    PIC 9(5).                       
008200     SKIP2                                                                
009100 01    FILLER                    PIC X(08) VALUE 'CCCCCCCC'.              
009200 01    WS-JFR-IDANSTNR.                                                   
009300   03  FILLER                    PIC X(3).                                
009400   03  WS-JFR-IDANSTNR-5         PIC X(5).                                
009500     EJECT                                                                
009600 77    WS-IDRADNR-ORD-TOM        PIC 9(4).                                
009700 77    WS-ADPACOMR               PIC X(2).                                
009800     EJECT                                                                
009900 01    NYCKEL-TYP                PIC S9(2)   COMP-3.                      
010000   88  GAMMAL-NYCKEL             VALUE +1.                                
010100   88  NY-NYCKEL                 VALUE +2.                                
010200     SKIP3                                                                
010300 01    FRAN-BILD                 PIC 9(4).                                
010400   88  FRAN-BILD-OK              VALUE 4312                               
010500                                       4321 4322 4323 4324 4325.          
010600   88  FRAN-BILD-EGEN            VALUE 4321.                              
010700     SKIP3                                                                
010800 01    WS-SLINGA-KLAR            PIC X(1).                                
010900   88  SLINGA-KLAR               VALUE 'J'.                               
011000     SKIP3                                                                
011100 01    WS-FIRST-TIME             PIC X(1).                                
011200   88  FIRST-TIME                VALUE 'J'.                               
011300     SKIP3                                                                
011400 01    FILLER                    PIC X(08) VALUE 'DDDDDDDD'.              
011500 01    WS-NYCKEL-NAESTA.                                                  
011600   03  WS-IDPRODNR-NYCKEL        PIC 9(7).                                
011700   03  WS-IDANSTNR-NYCKEL        PIC 9(5).                                
011800   03  WS-IDRADNR-ORD-TOM-NYCKEL PIC 9(4).                                
011900     SKIP2                                                                
012000 01    DYNAMISKA-SUBPROGRAM.                                              
012100   03  CBLTDLI                   PIC X(8) VALUE 'CBLTDLI '.               
012200   03  FELLOG                    PIC X(8) VALUE 'FELLOG  '.               
012400   03  ABEND                     PIC X(8) VALUE 'ABEND   '.               
012500   03  WZ01SUB                   PIC X(8) VALUE 'WZ01SUB '.               
012600     SKIP2                                                                
012700*    --- PARAMETERS TO ABEND                                              
012800                                                                          
012900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013200     SKIP2                                                                
013600 77    SW-IDPRODNR               PIC X.                                   
013700 77    SW-ORDERID                PIC X.                                   
013800     EJECT                                                                
013900 01    FILLER                    PIC X(08) VALUE 'DLI-KEYS'.              
014000 01    NYCKLAR-TILL-DLI.                                                  
014100   03    W-IDPRODNR-X.                                                    
014200     05    W-IDPRODNR            PIC S9(7)    COMP-3.                     
014300     SKIP2                                                                
014400   03    W-IDRADNR-ORD-X.                                                 
014500     05    W-IDRADNR-ORD         PIC S9(5)    COMP-3.                     
014600     SKIP2                                                                
014700   03    W-IDRADNR-ORAPP-X.                                               
014800     05    W-IDRADNR-ORAPP       PIC S9(5)    COMP-3.                     
014900     SKIP2                                                                
015000   03    W-WDE4A1-KUNDORDER-X.                                            
015100     05    W-4A1-IDDISTR             PIC S9(5)    COMP-3.                 
015200     05    W-4A1-IDKUNDNR            PIC S9(7)    COMP-3.                 
015300     05    W-4A1-IDKUNDRF.                                                
015400       07  W-4A1-IDORDNR             PIC X(5).                            
015500       07  FILLER                    PIC X(5)     VALUE SPACE.            
015600     SKIP2                                                                
015700   03    W-WDE401-KUNDORDER-X.                                            
015800     05    W-401-IDDISTR             PIC S9(5)    COMP-3.                 
015900     05    W-401-IDKUNDNR            PIC S9(7)    COMP-3.                 
016000     05    W-401-IDKUNDRF.                                                
016100       07  W-401-IDORDNR             PIC X(5).                            
016200       07  FILLER                    PIC X(5)     VALUE SPACE.            
016300     05    W-401-IDPRODNR            PIC S9(7)    COMP-3.                 
016400     05    W-401-IDPLKLST            PIC S9(3)    COMP-3.                 
016500     SKIP2                                                                
016600   03    W-OLD-KUNDORDER-X.                                               
016700     05    W-OLD-IDDISTR             PIC S9(5)    COMP-3.                 
016800     05    W-OLD-IDKUNDNR            PIC S9(7)    COMP-3.                 
016900     05    W-OLD-IDKUNDRF.                                                
017000       07  W-OLD-IDORDNR             PIC X(5).                            
017100       07  FILLER                    PIC X(5).                            
017200     05    W-OLD-IDPRODNR            PIC S9(7)    COMP-3.                 
017300     05    W-OLD-IDPLKLST            PIC S9(3)    COMP-3.                 
017400     SKIP2                                                                
017500   03    W-WDE420-KEYSEQ-X.                                               
017600     05    W-420-IDPRODNR            PIC S9(7)    COMP-3.                 
017700     05    W-420-IDPURAD             PIC S9(5)    COMP-3.                 
017800     SKIP2                                                                
017900   03    W-WDE420-KEYSEQ-MIN-X.                                           
018000     05    W-420-IDPRODNR-MIN        PIC S9(7)    COMP-3.                 
018100     05    W-420-IDPURAD-MIN         PIC S9(5)    COMP-3.                 
018200     SKIP2                                                                
018300   03    W-WDE420-KEYSEQ-MAX-X.                                           
018400     05    W-420-IDPRODNR-MAX        PIC S9(7)    COMP-3.                 
018500     05    W-420-IDPURAD-MAX         PIC S9(5)    COMP-3.                 
018600   03    W-WDE601-IDPRODNR-X.                                             
018700     05    W-601-IDPRODNR            PIC S9(7)    COMP-3.                 
018800     SKIP2                                                                
018900 01    FILLER                    PIC X(08) VALUE 'MESSAGE:'.              
019000 01  MESSAGE-CODES.                                                       
019100     03  ERR-FIELD-IS-INVALID    PIC X(3)    VALUE '023'.                 
019200     03  ERR-INVALID-KEY-FILEDS  PIC X(3)    VALUE '043'.                 
019300     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '304'.                 
019400     03  ERR-ORDER-NOT-SPLIT     PIC X(3)    VALUE '161'.                 
019500     03  ERR-ORDER-TOTALLY-REPORTED  PIC X(3) VALUE '146'.                
019600     03  ERR-ORDER-WRONG-STATUS  PIC X(3)    VALUE '273'.                 
019700     SKIP2                                                                
019800 01    FILLER                    PIC X(08) VALUE 'MEDDELA:'.              
019900 01    MEDDELANDE.                                                        
020000   03    FEL1.                                                            
020100     05    FILLER                PIC X(40)   VALUE                        
020200             '701. ORDERN SAKNAS                     '.                   
020300     05    FILLER                PIC X(40)   VALUE                        
020400             '701. ORDER MISSING                     '.                   
020500   03    FILLER REDEFINES FEL1.                                           
020600     05    FEL-1 OCCURS 2        PIC X(40).                               
020700     SKIP2                                                                
020800   03    FEL2.                                                            
020900     05    FILLER                PIC X(40)   VALUE                        
021000             '783. ORDERN EJ DELAD                   '.                   
021100     05    FILLER                PIC X(40)   VALUE                        
021200             '783. ORDER HAS NOT BEEN SPLIT          '.                   
021300   03    FILLER REDEFINES FEL2.                                           
021400     05    FEL-2 OCCURS 2        PIC X(40).                               
021500     SKIP2                                                                
021600   03    FEL3.                                                            
021700     05    FILLER                PIC X(40)   VALUE                        
021800             '710. ORDERN FÄRDIGRAPPORTERAD          '.                   
021900     05    FILLER                PIC X(40)   VALUE                        
022000             '710 ORDER TOTALLY REPORTED             '.                   
022100   03    FILLER REDEFINES FEL3.                                           
022200     05    FEL-3 OCCURS 2        PIC X(40).                               
022300     SKIP2                                                                
022400   03    FEL4.                                                            
022500     05    FILLER                PIC X(40)   VALUE                        
022600             '784. BLÄDDRING EJ TILLÅTEN             '.                   
022700     05    FILLER                PIC X(40)   VALUE                        
022800             '784. SCROLLING NOT ALLOWED             '.                   
022900   03    FILLER REDEFINES FEL4.                                           
023000     05    FEL-4 OCCURS 2        PIC X(40).                               
023100     EJECT                                                                
023200   03    FEL5.                                                            
023300     05    FILLER                PIC X(40)   VALUE                        
023400             '749. FEL NYCKEL                        '.                   
023500     05    FILLER                PIC X(40)   VALUE                        
023600             '749. WRONG KEY                         '.                   
023700   03    FILLER REDEFINES FEL5.                                           
023800     05    FEL-5 OCCURS 2        PIC X(40).                               
023900     SKIP2                                                                
024000   03    MED1.                                                            
024100     05    FILLER                PIC X(40)   VALUE                        
024200             '778. FLER RADER FINNS                  '.                   
024300     05    FILLER                PIC X(40)   VALUE                        
024400             '778. MORE LINES                        '.                   
024500   03    FILLER REDEFINES MED1.                                           
024600     05    MED-1 OCCURS 2        PIC X(40).                               
024700     EJECT                                                                
024800******************************************************************        
024900*                                                                         
025200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
025300     SKIP3                                                                
025400*01  -COPY WZ01SUB                                                        
025500     SKIP3                                                                
025600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
025700     SKIP3                                                                
025800 01  REQU-AREA.                                                           
025900*    03  -COPY WZ01REQU                                                   
026000*    03  -COPY WL0116I1                                                   
026100     SKIP3                                                                
026200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
026300     SKIP3                                                                
026400 01  RESP-AREA.                                                           
026500*    03  -COPY WZ01RESP                                                   
026600*    03  -COPY WL0116O1                                                   
026700******************************************************************        
026800*                                                                         
026900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027000*                                                                         
027100 01    IMS-WS.                                                            
027200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
027300     SKIP3                                                                
027400*                        **** STATUS-KOD FRÅN IMS                         
027500   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
027600     88    KUNDORDER-SEK-FINNS              VALUE '  '.                   
027700     88    KUNDORDER-SEK-SAKNAS             VALUE 'GE' 'GB'.              
027800   03    STATUS-WS               PIC XX.                                  
027900     88    SEGMENT-FINNS                    VALUE '  '.                   
028000     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
028100     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
028200     88    SLUT-PA-BASEN                    VALUE 'GB'.                   
028300     SKIP3                                                                
028400   03    GODK-STATUSKODER.                                                
028500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
028600     SKIP3                                                                
028700 01    SSA1                      PIC X(64).                               
028800 01    SSA2                      PIC X(64).                               
028900 01    SSA3                      PIC X(64).                               
029000 01    SSA4                      PIC X(64).                               
029100     SKIP2                                                                
029200*                            IMS FUNKTIONSKODER                           
029300*01    -COPY W0003                                                        
029400     SKIP2                                                                
029500 01    FILLER                    PIC X(08) VALUE 'DLIOAREA'.              
029600*                            DLI INPUT-OUTPUT AREA                        
029700 01    DLI-IO-AREA.                                                       
029800   03    IO-AREA                 PIC X(510)  VALUE SPACE.                 
029900     SKIP2                                                                
030000*  03    WDE401   -COPY WDE401              -RED IO-AREA.                 
030100     SKIP2                                                                
030200*  03    WDE411   -COPY WDE411              -RED IO-AREA.                 
030300     SKIP2                                                                
030400*  03    WDE601   -COPY WDE601              -RED IO-AREA.                 
030500     SKIP2                                                                
030600 01    FILLER                    PIC X(08) VALUE 'LINKAGE:'.              
030700 LINKAGE SECTION.                                                         
030800*01    -COPY W0009     -PRE MSG-                                          
030900     SKIP2                                                                
031000*01    -COPY W0008     -PRE WDE4-                                         
031100     05  FILLER                  PIC X.                                   
031200     SKIP2                                                                
031300*01    -COPY W0008     -PRE WDE42-                                        
031400     05  FILLER                  PIC X.                                   
031500     SKIP2                                                                
031600*01    -COPY W0008     -PRE WDE43-                                        
031700     05  FILLER                  PIC X.                                   
031800     SKIP2                                                                
031900*01    -COPY W0008     -PRE WDE6-                                         
032000     05  FILLER                  PIC X.                                   
032100     SKIP2                                                                
032200 PROCEDURE DIVISION USING MSG-PCB                                         
032300                          WDE4-PCB WDE42-PCB WDE43-PCB WDE6-PCB.          
032400 MAIN SECTION.                                                            
032500     ENTRY 'DLITCBL' USING MSG-PCB                                        
032600                           WDE4-PCB WDE42-PCB WDE43-PCB WDE6-PCB.         
032700     SKIP2                                                                
032800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
032900     IF SUB-KDRC = 0                                                      
033000       IF REQU-KDPGMACT = 'S'                                             
033100                                                                          
033200        PERFORM A-INIT-SPARA-INPUT                                        
033300        PERFORM B-KOLLA-INPUT                                             
033400        IF WS-KDFEL = ZERO                                                
033500           PERFORM C-HAEMTA-STARTNYCKEL                                   
033600        END-IF                                                            
033700        IF WS-KDFEL = ZERO                                                
033900           PERFORM D-BEHANDLA-RADER                                       
034000        END-IF                                                            
034100        IF WS-KDFEL > ZERO                                                
034200           PERFORM J-HAMTA-MEDDELANDE                                     
034600        END-IF                                                            
034700       END-IF                                                             
034800       PERFORM S02-RETURN-RESPONSE                                        
034900     END-IF                                                               
035000                                                                          
035100     MOVE +0 TO RETURN-CODE                                               
035200     GOBACK                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 A-INIT-SPARA-INPUT SECTION.                                              
035600     MOVE 'STA A-INIT        ' TO PGM-POS                                 
035700                                                                          
035800     MOVE ZERO                 TO WS-KDFEL                                
035900                                                                          
036000     MOVE ALL '+'              TO RESP-AREA                               
036100     MOVE 001                  TO RESP-IDMSGVER                           
036200     MOVE SPACE                TO RESP-IDMSG-ERROR                        
036300                                  RESP-IDMSG-INFO                         
036400                                  RESP-IDELMT-ERROR                       
036500     MOVE ZERO TO RESP-KVRADER                                            
036600     PERFORM AB-INIT-NYCKLAR                                              
036700                                                                          
037600     MOVE 'END A-INIT        ' TO PGM-POS                                 
037700     .                                                                    
037800     EJECT                                                                
037900 AB-INIT-NYCKLAR SECTION.                                                 
038000     MOVE 'STA AB-INIT-NYCKLAR   ' TO PGM-POS                             
038100                                                                          
038200     MOVE +2 TO SPRAK-INDX                                                
038300                                                                          
038400     MOVE NEJ              TO SW-IDPRODNR                                 
038500                              SW-ORDERID                                  
038600     MOVE 1                TO NYCKEL-TYP                                  
038800*                                                                         
038900     IF REQU-IDPRODNR-KEY NOT = ALL '+'                                   
039000         MOVE 2                      TO NYCKEL-TYP                        
039100         MOVE JA                     TO SW-IDPRODNR                       
039110         MOVE REQU-IDPRODNR-KEY      TO WS-IDPRODNR                       
039500     END-IF                                                               
039700*                                                                         
039800     IF REQU-IDDISTR-KEY NOT = ALL '+'                                    
039900         MOVE 2                      TO NYCKEL-TYP                        
040000         MOVE JA                     TO SW-ORDERID                        
040010         MOVE REQU-IDDISTR-KEY       TO WS-IDDISTR                        
040400     END-IF                                                               
040600*                                                                         
040700     IF REQU-IDORDNR-KEY NOT = ALL '+'                                    
040800         MOVE 2                      TO NYCKEL-TYP                        
040900         MOVE JA                     TO SW-ORDERID                        
040910         MOVE REQU-IDORDNR-KEY       TO WS-IDORDNR                        
040920         MOVE WS-IDORDNR             TO WS-IDKUNDRF                       
041400     END-IF                                                               
041700*                                                                         
041800     IF REQU-IDKUNDNR-KEY NOT = ALL '+'                                   
041900         MOVE 2                      TO NYCKEL-TYP                        
042000         MOVE JA                     TO SW-ORDERID                        
042010         MOVE REQU-IDKUNDNR-KEY      TO WS-IDKUNDNR                       
042400     END-IF                                                               
042600*                                                                         
042700*       --- CHECK INPUT WAREHOUSE IDENTIFIER                              
042800*                                                                         
042900     MOVE REQU-IDDC-KEY        TO RESP-IDDC-KEY                           
043000                                                                          
043100     MOVE 'END AB-INIT-NYCKLAR   ' TO PGM-POS                             
043200     .                                                                    
043300     EJECT                                                                
043400 B-KOLLA-INPUT SECTION.                                                   
043500     MOVE 'STA B-KOLLA-INPUT     ' TO PGM-POS                             
043600*                                                                         
043601     IF  REQU-IDDISTR-KEY = ALL '+'                                       
043602     AND REQU-IDKUNDNR-KEY = ALL '+'                                      
043603     AND REQU-IDORDNR-KEY = ALL '+'                                       
043610     AND REQU-IDPRODNR-KEY = ALL '+'                                      
043611         MOVE 'ALLKEYS1'             TO RESP-IDELMT-ERROR                 
043612         MOVE 5                      TO WS-KDFEL                          
043620     ELSE                                                                 
043700       IF WS-IDPRODNR NUMERIC                                             
043710       OR REQU-IDPRODNR-KEY = ALL '+'                                     
043800         IF WS-IDPRODNR > ZERO                                            
043900         AND SW-ORDERID NOT = JA                                          
044000             MOVE JA                 TO SW-IDPRODNR                       
044010             MOVE REQU-IDPRODNR-KEY  TO WS-IDPRODNR-NYCKEL                
044100         END-IF                                                           
044200       ELSE                                                               
044300         MOVE 'IDPRODNR'             TO RESP-IDELMT-ERROR                 
044400         MOVE 5                      TO WS-KDFEL                          
044500       END-IF                                                             
044600*                                                                         
044700        IF WS-IDKUNDNR NUMERIC                                            
044710        OR REQU-IDKUNDNR-KEY = ALL '+'                                    
044900            CONTINUE                                                      
045100        ELSE                                                              
045200          MOVE 'IDKUNDNR'            TO RESP-IDELMT-ERROR                 
045300          MOVE 5                     TO WS-KDFEL                          
045400        END-IF                                                            
045500*                                                                         
045600        IF WS-IDDISTR NUMERIC                                             
045610        OR REQU-IDDISTR-KEY = ALL '+'                                     
045800            CONTINUE                                                      
046000        ELSE                                                              
046100          MOVE 'IDDISTR'             TO RESP-IDELMT-ERROR                 
046200          MOVE 5                     TO WS-KDFEL                          
046300        END-IF                                                            
046400*                                                                         
046500        IF WS-IDORDNR NUMERIC                                             
046510        OR REQU-IDORDNR-KEY = ALL '+'                                     
046700            CONTINUE                                                      
046900        ELSE                                                              
047000          MOVE 'IDORDNR'             TO RESP-IDELMT-ERROR                 
047100          MOVE 5                     TO WS-KDFEL                          
047200        END-IF                                                            
047220     END-IF                                                               
047300*                                                                         
048200     IF WS-KDFEL > ZERO                                                   
048201       CONTINUE                                                           
048210     ELSE                                                                 
048300       IF SW-IDPRODNR = JA                                                
048310          MOVE IDPRODNR-WS      TO W-IDPRODNR                             
048320          MOVE IDPRODNR-WS      TO WS-IDPRODNR-XTRA                       
048330          MOVE WS-IDPRODNR-XTRA TO WS-IDPRODNR                            
048400          MOVE NEJ              TO SW-ORDERID                             
048800       ELSE                                                               
048900          MOVE JA               TO SW-ORDERID                             
050500       END-IF                                                             
050600       MOVE ZERO                TO W-IDRADNR-ORD                          
050700                                     W-IDRADNR-ORAPP                      
051100       MOVE IDPRODNR-WS         TO W-IDPRODNR                             
051700                                                                          
051810       IF REQU-IDPRODNR-KEY NOT = ALL '+'                                 
051900         MOVE WS-IDPRODNR TO RESP-IDPRODNR-KEY                            
051910       END-IF                                                             
052210       IF REQU-IDKUNDNR-KEY NOT = ALL '+'                                 
052300         MOVE WS-IDKUNDNR TO RESP-IDKUNDNR-KEY                            
052310       END-IF                                                             
052610       IF REQU-IDDISTR-KEY NOT = ALL '+'                                  
052700         MOVE WS-IDDISTR TO RESP-IDDISTR-KEY                              
052710       END-IF                                                             
053010       IF REQU-IDORDNR-KEY NOT = ALL '+'                                  
053100         MOVE WS-IDORDNR TO RESP-IDORDNR-KEY                              
053110       END-IF                                                             
053120     END-IF                                                               
053900     MOVE 'END B-KOLLA-INPUT     ' TO PGM-POS                             
054000     .                                                                    
054100     EJECT                                                                
055900 C-HAEMTA-STARTNYCKEL SECTION.                                            
056000     MOVE 'STA C-HAEMTA-STARTNYCKEL       ' TO PGM-POS                    
056100                                                                          
058800     IF  SW-ORDERID = NEJ                                                 
059000        MOVE IDPRODNR-WS          TO W-420-IDPRODNR-MIN                   
059100                                     W-420-IDPRODNR-MAX                   
059200        MOVE 1                    TO W-420-IDPURAD-MIN                    
059300        MOVE 99999                TO W-420-IDPURAD-MAX                    
059400        PERFORM IMS-GU-KUNDORDER-SEK-INV-INT                              
059500                                                                          
059600        IF SEGMENT-FINNS                                                  
059700           MOVE KORD-IDDISTR      TO IDDISTR-WS                           
059710                                     W-OLD-IDDISTR                        
059800           MOVE KORD-IDKUNDNR     TO IDKUNDNR-WS                          
059810                                     W-OLD-IDKUNDNR                       
059900           MOVE KORD-IDKUNDRF     TO WS-IDKUNDRF                          
059910                                     W-OLD-IDKUNDRF                       
059920           MOVE KORD-IDPRODNR     TO W-OLD-IDPRODNR                       
059930           MOVE KORD-IDPLKLST     TO W-OLD-IDPLKLST                       
060000        ELSE                                                              
060100           MOVE 1                 TO WS-KDFEL                             
060110           MOVE 'IDORDN3'         TO RESP-IDELMT-ERROR                    
060130           MOVE WS-IDPRODNR-TEST  TO REQU-IDPRODNR-KEY                    
060150           MOVE ZERO              TO W-OLD-IDDISTR                        
060170           MOVE ZERO              TO W-OLD-IDKUNDNR                       
060190           MOVE ZERO              TO W-OLD-IDKUNDRF                       
060191           MOVE ZERO              TO W-OLD-IDPRODNR                       
060192           MOVE ZERO              TO W-OLD-IDPLKLST                       
060200        END-IF                                                            
060300     ELSE                                                                 
060303       MOVE ZERO                  TO W-OLD-IDDISTR                        
060304       MOVE ZERO                  TO W-OLD-IDKUNDNR                       
060305       MOVE ZERO                  TO W-OLD-IDKUNDRF                       
060306       MOVE ZERO                  TO W-OLD-IDPRODNR                       
060307       MOVE ZERO                  TO W-OLD-IDPLKLST                       
060310     END-IF                                                               
060400*                                                                         
060500     IF WS-KDFEL = ZERO                                                   
060600        MOVE IDDISTR-WS           TO W-4A1-IDDISTR                        
060700        MOVE IDKUNDNR-WS          TO W-4A1-IDKUNDNR                       
060800        MOVE WS-IDKUNDRF          TO W-4A1-IDKUNDRF                       
060900        PERFORM IMS-GU-KUNDORDER-SEK                                      
061100        IF KUNDORDER-SEK-FINNS                                            
061200           PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                          
061300                         SLINGA-KLAR                                      
061400           MOVE KORD-IDDISTR      TO W-401-IDDISTR                        
061500           MOVE KORD-IDKUNDNR     TO W-401-IDKUNDNR                       
061600           MOVE KORD-IDORDNR5     TO W-401-IDORDNR                        
061700           MOVE KORD-IDPRODNR     TO W-401-IDPRODNR                       
061800           MOVE KORD-IDPLKLST     TO W-401-IDPLKLST                       
061900           PERFORM IMS-GU-KUNDORDER                                       
062000                                                                          
062100           MOVE KORD-IDPRODNR     TO W-601-IDPRODNR                       
062200           PERFORM IMS-GU-IDPRODNR                                        
062300           IF SEGMENT-FINNS                                               
062400              IF SW-IDPRODNR = NEJ                                        
062500                 IF VORD-IDDC = REQU-IDDC-KEY                             
062600                    CONTINUE                                              
062700                 ELSE                                                     
062800                    SET SEGMENT-SAKNAS TO TRUE                            
062900                 END-IF                                                   
063000              ELSE                                                        
063100                 IF VORD-IDPRODNR = IDPRODNR-WS                           
063200                    CONTINUE                                              
063300                 ELSE                                                     
063400                    SET SEGMENT-SAKNAS TO TRUE                            
063500                 END-IF                                                   
063600              END-IF                                                      
063700           END-IF                                                         
063800*                                                                         
063900           IF SEGMENT-FINNS                                               
064000              IF VORD-IDDC     = REQU-IDDC-KEY                            
064100                 MOVE VORD-IDPRODNR  TO IDPRODNR-WS                       
064200                 MOVE VORD-KDORDSTA  TO WS-SPAR-KDORDSTA                  
064300                 MOVE 'J'            TO WS-SLINGA-KLAR                    
064400              END-IF                                                      
064500           END-IF                                                         
064600*                                                                         
064700           PERFORM IMS-GN-KUNDORDER-SEK                                   
064800           END-PERFORM                                                    
064900           IF NOT SLINGA-KLAR                                             
065000              MOVE 1                 TO WS-KDFEL                          
065100              MOVE 'IDORDN4'         TO RESP-IDELMT-ERROR                 
065200           END-IF                                                         
065300        ELSE                                                              
065400           MOVE 1                    TO WS-KDFEL                          
065500           MOVE 'IDORDN5'            TO RESP-IDELMT-ERROR                 
065600        END-IF                                                            
065700     END-IF                                                               
065800     MOVE 'END C-HAEMTA-STARTNYCKEL       ' TO PGM-POS                    
065900     .                                                                    
066000     EJECT                                                                
066100 D-BEHANDLA-RADER        SECTION.                                         
066200     MOVE 'STA D-BEHANDLA-RADER           ' TO PGM-POS                    
066300                                                                          
066400     MOVE 'J'                              TO WS-FIRST-TIME               
066500     MOVE 1                                TO WS-RAD-IND                  
066600*                                                                         
066700     MOVE IDDISTR-WS                       TO W-4A1-IDDISTR               
066800     MOVE IDKUNDNR-WS                      TO W-4A1-IDKUNDNR              
066900     MOVE WS-IDKUNDRF                      TO W-4A1-IDKUNDRF              
067000     PERFORM IMS-GU-KUNDORDER-SEK                                         
067100*                                                                         
067200     IF KUNDORDER-SEK-FINNS                                               
067300        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
067400                      WS-RAD-IND > WS-RESP-MAX-500                        
067500        MOVE KORD-IDDISTR                  TO W-401-IDDISTR               
067600        MOVE KORD-IDKUNDNR                 TO W-401-IDKUNDNR              
067700        MOVE KORD-IDORDNR5                 TO W-401-IDORDNR               
067800        MOVE KORD-IDPRODNR                 TO W-401-IDPRODNR              
067900        MOVE KORD-IDPLKLST                 TO W-401-IDPLKLST              
068000        PERFORM IMS-GU-KUNDORDER                                          
068100*                                                                         
068200        MOVE KORD-IDPRODNR                 TO WS-JFR-IDPRODNR             
068300        MOVE KORD-IDUSER                   TO WS-JFR-IDANSTNR             
068400        IF IDPRODNR-WS = WS-JFR-IDPRODNR                                  
068500           MOVE KORD-IDPRODNR              TO WS-IDPRODNR-NYCKEL          
068600           IF WS-SPAR-KDORDSTA > 3                                        
068700                                                                          
068800              MOVE 10 TO WS-KDFEL                                         
068900              MOVE 'IDPRODNR'              TO RESP-IDELMT-ERROR           
069000           ELSE                                                           
069100             IF WS-KDFEL = ZERO                                           
069200                PERFORM DA-LAGG-UT-RADER                                  
069300             END-IF                                                       
069400           END-IF                                                         
069500        END-IF                                                            
069600*                                                                         
069700        PERFORM IMS-GN-KUNDORDER-SEK                                      
069800        END-PERFORM                                                       
069801                                                                          
069802        MOVE WS-RAD-IND                    TO WS-KVRADER                  
069820        COMPUTE WS-KVRADER = WS-KVRADER - 1                               
069830        END-COMPUTE                                                       
069840        MOVE WS-KVRADER                    TO RESP-KVRADER                
069850*                                                                         
069900     ELSE                                                                 
070000        MOVE 1 TO WS-KDFEL                                                
070100        MOVE 'IDORDN6'            TO RESP-IDELMT-ERROR                    
070200     END-IF                                                               
070300     MOVE 'END D-BEHANDLA-RADER           ' TO PGM-POS                    
070400     .                                                                    
070500     EJECT                                                                
070600 DA-LAGG-UT-RADER        SECTION.                                         
070700     MOVE 'STA DA-LAGG-UT-RADER           ' TO PGM-POS                    
070800                                                                          
071600     IF  FIRST-TIME                                                       
072000        MOVE 'N'                  TO WS-FIRST-TIME                        
072100        PERFORM IMS-GNP-RAD                                               
072200*                                                                         
072300        IF SEGMENT-FINNS                                                  
072400           COMPUTE WS-SPAR-IDPURAD = ORAD-IDPURAD -                       
072500                                     1                                    
072510           END-COMPUTE                                                    
072600           MOVE ORAD-IDPURAD       TO WS-START-IDPURAD                    
072700                                      WS-STOPP-IDPURAD                    
072800*                                                                         
072900           PERFORM UNTIL SEGMENT-SAKNAS OR                                
073000                         WS-RAD-IND > WS-RESP-MAX-500                     
073400           IF (ORAD-IDPURAD > WS-IDRADNR-ORD-TOM-NYCKEL)                  
073500              IF ORAD-KDRADSTA < 4                                        
073600                 IF ORAD-IDPURAD = WS-SPAR-IDPURAD + 1                    
073700                    MOVE ORAD-IDPURAD TO WS-SPAR-IDPURAD                  
073800                                         WS-STOPP-IDPURAD                 
073900                    COMPUTE WS-KVORAPP = WS-KVORAPP +                     
074000                                         ORAD-KVAVBART -                  
074100                                         ORAD-KVLEVART                    
074200                    PERFORM IMS-GNP-RAD                                   
074300                 ELSE                                                     
074400                    IF WS-KVORAPP > ZERO                                  
074500                       PERFORM S10-SKRIV-RAD                              
074600                       ADD 1             TO WS-RAD-IND                    
074700                       COMPUTE WS-SPAR-IDPURAD = ORAD-IDPURAD -           
074800                                                 1                        
074810                       END-COMPUTE                                        
074900                       MOVE ORAD-IDPURAD TO WS-START-IDPURAD              
075000                                            WS-STOPP-IDPURAD              
075100                                            WS-SPAR-IDPURAD               
075200                       COMPUTE WS-KVORAPP = WS-KVORAPP +                  
075300                                            ORAD-KVAVBART -               
075400                                            ORAD-KVLEVART                 
075500                    END-IF                                                
075600                    PERFORM IMS-GNP-RAD                                   
075700                 END-IF                                                   
075800              ELSE                                                        
075900                 IF WS-KVORAPP > ZERO                                     
076000                    PERFORM S10-SKRIV-RAD                                 
076100                    ADD 1                TO WS-RAD-IND                    
076200                 END-IF                                                   
076300                 PERFORM IMS-GNP-RAD                                      
076400                 IF SEGMENT-FINNS                                         
076500                    COMPUTE WS-SPAR-IDPURAD = ORAD-IDPURAD -              
076600                                              1                           
076610                    END-COMPUTE                                           
076700                    MOVE ORAD-IDPURAD TO WS-START-IDPURAD                 
076800                                         WS-STOPP-IDPURAD                 
076900                 END-IF                                                   
077000              END-IF                                                      
077100           ELSE                                                           
077200              PERFORM IMS-GNP-RAD                                         
077300              IF SEGMENT-FINNS                                            
077400                 COMPUTE WS-SPAR-IDPURAD = ORAD-IDPURAD -                 
077500                                           1                              
077510                 END-COMPUTE                                              
077600                 MOVE ORAD-IDPURAD TO WS-START-IDPURAD                    
077700                                      WS-STOPP-IDPURAD                    
077800              END-IF                                                      
077900           END-IF                                                         
078000           END-PERFORM                                                    
078100        END-IF                                                            
078200     END-IF                                                               
078300*                                                                         
078400     IF SEGMENT-SAKNAS AND                                                
078500        WS-RAD-IND NOT > WS-RESP-MAX-500                                  
078600        IF WS-KVORAPP > ZERO                                              
078700           PERFORM S10-SKRIV-RAD                                          
078800           ADD 1                TO WS-RAD-IND                             
078900           COMPUTE WS-SPAR-IDPURAD = ORAD-IDPURAD -                       
079000                                     1                                    
079010           END-COMPUTE                                                    
079100           MOVE ORAD-IDPURAD TO WS-START-IDPURAD                          
079200                                WS-STOPP-IDPURAD                          
079300        END-IF                                                            
079400     END-IF                                                               
079410     IF WS-RAD-IND = 1                                                    
079411       MOVE 293              TO RESP-IDMSG-INFO                           
079420     END-IF                                                               
079500*                                                                         
079700     MOVE 'END DA-LAGG-UT-RADER           ' TO PGM-POS                    
079800     .                                                                    
079900     EJECT                                                                
081200 J-HAMTA-MEDDELANDE SECTION.                                              
081300     MOVE 'STA J-HAMTA-MEDDELANDE         ' TO PGM-POS                    
081400                                                                          
081500     EVALUATE WS-KDFEL                                                    
081700       WHEN  1 MOVE ERR-ORDER-MISSING  TO RESP-IDMSG-ERROR                
081900       WHEN  2 MOVE ERR-ORDER-NOT-SPLIT TO RESP-IDMSG-ERROR               
082100       WHEN  3 MOVE ERR-ORDER-TOTALLY-REPORTED                            
082200                       TO RESP-IDMSG-ERROR                                
082600       WHEN  5 MOVE ERR-INVALID-KEY-FILEDS TO RESP-IDMSG-ERROR            
082700       WHEN 10 MOVE ERR-ORDER-WRONG-STATUS TO RESP-IDMSG-ERROR            
082900       WHEN 15 MOVE MED-1 (SPRAK-INDX) TO RESP-IDMSG-ERROR                
083000     END-EVALUATE                                                         
083100     MOVE 'END J-HAMTA-MEDDELANDE         ' TO PGM-POS                    
083200     .                                                                    
083300     EJECT                                                                
083400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
083500     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
083600                                                                          
083700     MOVE 'GETARG'               TO SUB-KDFUNC                            
083800     MOVE 'CARPARTS.LDC.UNREPORTEDLINES'     TO SUB-ADDISPABS             
083900     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
084000                                                                          
084100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
084200                                                                          
084300     IF SUB-KDRC > 0                                                      
084400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
084500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
084600       DELIMITED BY SIZE INTO FEL-TEXT                                    
084700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
084800     END-IF                                                               
084900     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
085000     .                                                                    
085100     SKIP3                                                                
085200 S02-RETURN-RESPONSE SECTION.                                             
085300     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
085400                                                                          
085500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
085600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
085700                                                                          
085800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
085900                                                                          
086000     IF SUB-KDRC > 0                                                      
086100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
086200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
086300       DELIMITED BY SIZE INTO FEL-TEXT                                    
086400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
086500     END-IF                                                               
086600     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
086700     .                                                                    
086800     EJECT                                                                
086900 S10-SKRIV-RAD SECTION.                                                   
087000                                                                          
087800     MOVE WS-ADPACOMR       TO RESP-ADPACOMR        (WS-RAD-IND)          
087900     MOVE WS-START-IDPURAD  TO RESP-IDRADNR-ORD-FROM (WS-RAD-IND)         
088000     MOVE WS-STOPP-IDPURAD  TO WS-IDRADNR-ORD-TOM-NYCKEL                  
088100                               RESP-IDRADNR-ORD-TOM (WS-RAD-IND)          
088200     MOVE WS-KVORAPP        TO RESP-KVORAPP         (WS-RAD-IND)          
088300     MOVE ZERO              TO WS-KVORAPP                                 
088400     MOVE WS-JFR-IDANSTNR-5 TO RESP-IDANSTNR        (WS-RAD-IND)          
088500     .                                                                    
088600     EJECT                                                                
088700* IMS SEKTIONER                                                           
088800     SKIP3                                                                
088900 IMS-GU-KUNDORDER SECTION.                                                
089000     MOVE 'IMS-GU-KUNDORDER      ' TO PGM-POS                             
089100                                                                          
089200     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
089300            DELIMITED BY SIZE INTO SSA1                                   
089400     MOVE '    ' TO GODK-STATUSKODER                                      
089500     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA SSA1                      
089600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
089700     PERFORM IMS-STATUSKONTROLL                                           
089800     SKIP3                                                                
089900     .                                                                    
090000 IMS-GNP-RAD  SECTION.                                                    
090100     MOVE 'IMS-GNP-RAD           ' TO PGM-POS                             
090200                                                                          
090300     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
090400            DELIMITED BY SIZE INTO SSA1                                   
090500     MOVE 'WDE411   ' TO SSA2                                             
090600     MOVE '  GE' TO GODK-STATUSKODER                                      
090700     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-AREA SSA1 SSA2                
090800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
090900     PERFORM IMS-STATUSKONTROLL                                           
091000     SKIP3                                                                
091100     .                                                                    
091200 IMS-GU-IDPRODNR SECTION.                                                 
091300     MOVE 'IMS-GU-IDPRODNR       ' TO PGM-POS                             
091400                                                                          
091500     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
091600            DELIMITED BY SIZE INTO SSA1                                   
091700     MOVE '  GE' TO GODK-STATUSKODER                                      
091800     CALL CBLTDLI USING GU  WDE6-PCB DLI-IO-AREA SSA1                     
091900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200     EJECT                                                                
092300 IMS-GU-KUNDORDER-SEK SECTION.                                            
092400     MOVE 'IMS-GU-KUNDORDER-SEK  ' TO PGM-POS                             
092500                                                                          
092600     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
092700            DELIMITED BY SIZE INTO SSA1                                   
092800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
092900     CALL CBLTDLI USING GU WDE42-PCB DLI-IO-AREA SSA1                     
093000     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
093100                               STATUS-KUNDORDER-SEK-WS                    
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     SKIP3                                                                
093400     .                                                                    
093500 IMS-GN-KUNDORDER-SEK SECTION.                                            
093600     MOVE 'IMS-GN-KUNDORDER-SEK  ' TO PGM-POS                             
093700                                                                          
093800     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
093900            DELIMITED BY SIZE INTO SSA1                                   
094000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
094100     CALL CBLTDLI USING GN WDE42-PCB DLI-IO-AREA SSA1                     
094200     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
094300                               STATUS-KUNDORDER-SEK-WS                    
094400     PERFORM IMS-STATUSKONTROLL                                           
094500     .                                                                    
094600     EJECT                                                                
094700 IMS-GU-KUNDORDER-SEK-INV-INT SECTION.                                    
094800     MOVE 'KUNDORDER-SEK-INV-INT'  TO PGM-POS                             
094900                                                                          
095000     STRING 'WDE411  (WDE4BSEQ>=' W-WDE420-KEYSEQ-MIN-X                   
095100                    '&WDE4BSEQ<=' W-WDE420-KEYSEQ-MAX-X ')'               
095200            DELIMITED BY SIZE INTO SSA1                                   
095300     MOVE 'WDE401   ' TO SSA2                                             
095400     MOVE '  GE' TO GODK-STATUSKODER                                      
095500     CALL CBLTDLI USING GU WDE43-PCB DLI-IO-AREA SSA1 SSA2                
095600     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
095700     PERFORM IMS-STATUSKONTROLL                                           
095800     .                                                                    
095900     EJECT                                                                
097200 IMS-STATUSKONTROLL SECTION.                                              
097300     SKIP2                                                                
097400     SET STATUS-IX TO 1                                                   
097500     SEARCH GODK-STATUS AT END CALL FELLOG                                
097600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
097700     END-SEARCH                                                           
097800     CONTINUE                                                             
097900     .                                                                    
