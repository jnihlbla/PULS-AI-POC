000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4048200.                                                
000400*AUTHOR.         ROYNA LUND.                                              
000500*DATE-WRITTEN.   91/08/15.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        CROSSBILD KUNDREGISTREN WDB1 WDB2                                
001100*                                                                         
001200*        SÖKMÖJLIGHETER:                                                  
001300*        - SÖKA BETALARE OCH FÅ SAMTLIGA GODSMOTTAGARE                    
001400*          SOM FINNS UPPLAGDA MOT DENNA BETALARE.                         
001500*                                                                         
001600*          IDPARTNR ANGES HELT ELLER DELVIS.                              
001700*          DÅ VISAS SAMTLIGA BETALARE SOM BÖRJAR MED                      
001800*          ANGIVEN NYCKEL - IDPARTNR                                      
001900*                                                                         
002000*        - SÖKA GODSMOTTAGARE OCH FÅ DEN KUND SOM                         
002100*          FINNS REGISTRERAD PÅ GODSMOTTAGAREN.                           
002200*                                                                         
002300*          IDDISTR ELLER IDDISTR + IDKUNDNR ÄR                            
002400*          NYCKEL.                                                        
002500*                                                                         
002600*          OM IDKUNDNR ANGES VISAS GMT F O MED DETTA                      
002700*          NUMMER FÖR DISTRIKTET                                          
002800*                                                                         
002900*    INDATA.                                                              
003000*        TRANSAKTION: W4T482                                              
003100*        MID:         W4I48201                                            
003200*                                                                         
003300*    UTDATA.                                                              
003400*        MOD:         W4O48201                                            
003500                                                                          
003600     SKIP3                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(08)   VALUE 'W4048200'.            
004400                                                                          
004500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004700                                                                          
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000                                                                          
005100*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005200 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005300 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
005400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1132 COMP SYNC.        
005500                                                                          
005600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005700 77  WS-IDFTG                    PIC X(2)    VALUE SPACE.                 
005800*77  WS-FLVISA                   PIC X(1)    VALUE SPACE.                 
005900 77  WS-IDLANDX2                 PIC X(2)    VALUE SPACE.                 
006000 77  WS-IDBETNR                  PIC X(5)    VALUE SPACE.                 
006100 77  WS-IDPARTNR                 PIC X(9)    VALUE SPACE.                 
006200*77  WS-SPAR-IDPARTNR            PIC X(9)    VALUE SPACE.                 
006300*77  WS-SPAR-IDFTG               PIC 9(2)    VALUE ZERO.                  
006400 77  WS-SPAR-IDKUNDNR            PIC S9(7)   VALUE ZERO.                  
006500 77  WS-SPAR-IDDISTR             PIC S9(5)   VALUE ZERO.                  
006600                                                                          
006700 77  WS-IDDISTR                  PIC X(4)    VALUE ZERO.                  
006800 77  WS-IDKUNDNR                 PIC X(6)    VALUE ZERO.                  
006900                                                                          
007000                                                                          
007100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007200     88  NYCKLAR-OK                          VALUE 'J'.                   
007300     88  NYCKLAR-FEL                         VALUE 'N'.                   
007400                                                                          
007500 77  CHK-FLVISA-SW               PIC X       VALUE 'J'.                   
007600     88  FLVISA-OK                           VALUE 'J'.                   
007700     88  FLVISA-FEL                          VALUE 'N'.                   
007800                                                                          
007900 77  POST-FINNS-SW               PIC X       VALUE 'N'.                   
008000     88  POST-FINNS                          VALUE 'J'.                   
008100     88  POST-SAKNAS                         VALUE 'N'.                   
008200                                                                          
008300 77  NYCKEL-KEY-SW               PIC X(3)    VALUE SPACE.                 
008400     88  BET-KEY                             VALUE 'BET'.                 
008500     88  GMT-KEY                             VALUE 'GMT'.                 
008600     88  CTR-KEY                             VALUE 'CTR'.                 
008700                                                                          
008800 77  NYA-NYCKLAR-SW              PIC X       VALUE 'N'.                   
008900     88  NYA-NYCKLAR                         VALUE 'J'.                   
009000                                                                          
009100 77  SAMMA-BETALARE-SW           PIC X       VALUE 'N'.                   
009200     88  SAMMA-BETALARE                      VALUE 'J'.                   
009300     88  NY-BETALARE                         VALUE 'N'.                   
009400                                                                          
009500 77  GMT-SAKNAS-SW               PIC X       VALUE 'N'.                   
009600     88  GMT-SAKNAS                          VALUE 'J'.                   
009700 77  CTR-SAKNAS-SW               PIC X       VALUE 'N'.                   
009800     88  CTR-SAKNAS                          VALUE 'J'.                   
009900                                                                          
010000 77  ALLT-SW                     PIC X       VALUE 'N'.                   
010100     88  ALLT-OK                             VALUE 'J'.                   
010200                                                                          
010300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010400     88  EGEN-MID                            VALUE '4482'.                
010500     88  GODK-MID                            VALUE '4481' '4482'          
010600                                                   '4483' '4484'          
010700                                                   '4485' '4486'          
010800                                                   '4487' '4488'          
010900                                                   '4489'.                
011000     EJECT                                                                
011100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011200 01  GENERELLA-SUBPROGRAM.                                                
011300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011900     SKIP3                                                                
012000*01 -COPY WMSGINIT                                                        
012100     SKIP3                                                                
012200*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
012300*                                                                         
012400 01  SAVE-AREA.                                                           
012500      03 SAVE-IDTRANS          PIC X(4)    VALUE '4482'.                  
012600      03 SAVE-IDPARTNR-ENTER   PIC X(9).                                  
012700      03 SAVE-IDFTG-ENTER      PIC 9(2).                                  
012800      03 SAVE-IDMARKBO-ENTER   PIC X.                                     
012900      03 SAVE-IDDISTR-ENTER    PIC S9(5)           COMP-3.                
013000      03 SAVE-IDKUNDNR-ENTER   PIC S9(7)           COMP-3.                
013100      03 SAVE-IDPARTNR-NEXT    PIC X(9).                                  
013200      03 SAVE-IDFTG-NEXT       PIC 9(2).                                  
013300      03 SAVE-IDMARKBO-NEXT    PIC X.                                     
013400      03 SAVE-IDDISTR-NEXT     PIC S9(5)           COMP-3.                
013500      03 SAVE-IDKUNDNR-NEXT    PIC S9(7)           COMP-3.                
013600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013700*01 -COPY WMEDAREA                                                        
013800     SKIP3                                                                
013900 01  MESSAGE-CODES.                                                       
014000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014300     03  INF-LAST-PAGE-ALREADY   PIC X(3)    VALUE '115'.                 
014400     03  INF-FINAN-CUST-MISSING  PIC X(3)    VALUE '145'.                 
014500     03  INF-RECIEVER-MISSING    PIC X(3)    VALUE '146'.                 
014600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014700     EJECT                                                                
014800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014900*                                                                         
015000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015100     SKIP3                                                                
015200*01  MID -COPY W4I48201                                                   
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015500     SKIP3                                                                
015600*01  -COPY WMSGAREA                                                       
015700     EJECT                                                                
015800     03  MOD REDEFINES MSG-AREA.                                          
015900*      05  -COPY W4O48201                                                 
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016200     SKIP3                                                                
016300*01  -COPY WMFSAREA                                                       
016400     EJECT                                                                
016500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016600*                                                                         
016700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016800     SKIP2                                                                
016900 01  NYCKLAR-TILL-DLI.                                                    
017000     03  W-WDB101KY-X.                                                    
017100         05  W-WDB1-IDPARTNR      PIC X(9)    VALUE SPACE.                
017200         05  W-WDB1-IDFTG         PIC 9(2)    VALUE ZERO.                 
017300                                                                          
017400     03  W-WDB1KEY-LOW.                                                   
017500         05  W-IDPARTNR-B1-S-LOW  PIC X(9)    VALUE LOW-VALUE.            
017600         05  W-IDFTG-B1-S-LOW     PIC 9(2)    VALUE ZERO.                 
017700     03  W-WDB1KEY-HIGH.                                                  
017800         05  W-IDPARTNR-B1-S-HIGH PIC X(9)    VALUE HIGH-VALUE.           
017900         05  W-IDFTG-B1-S-HIGH    PIC 9(2)    VALUE 99.                   
018000                                                                          
018100     03  W-WDB1B1KY-LOW.                                                  
018200         05  W-IDLANDX2-LOW        PIC X(2)    VALUE SPACE.               
018300         05  W-IDMARKBO-LOW        PIC X(1)    VALUE LOW-VALUE.           
018400         05  W-IDPARTNR-LOW        PIC X(9)    VALUE LOW-VALUE.           
018500         05  W-IDFTG-LOW           PIC 9(2)    VALUE ZERO.                
018600                                                                          
018700     03  W-WDB1B1KY-HIGH.                                                 
018800         05  W-IDLANDX2-HIGH       PIC X(2)    VALUE SPACE.               
018900         05  W-IDMARKBO-HIGH       PIC X(1)    VALUE HIGH-VALUE.          
019000         05  W-IDPARTNR-HIGH       PIC X(9)    VALUE HIGH-VALUE.          
019100         05  W-IDFTG-HIGH          PIC 9(2)    VALUE 99.                  
019200                                                                          
019300     03  W-IDDISTR-X.                                                     
019400         05  W-IDDISTR            PIC S9(5)   VALUE +0  COMP-3.           
019500                                                                          
019600     03  W-WDB2KEY-LOW.                                                   
019700         05  W-IDDISTR-LOW        PIC S9(5)   VALUE +0  COMP-3.           
019800         05  W-IDKUNDNR-LOW       PIC S9(7)   VALUE +0  COMP-3.           
019900     03  W-WDB2KEY-HIGH.                                                  
020000         05  W-IDDISTR-HIGH       PIC S9(5)   VALUE +0  COMP-3.           
020100         05  FILLER               PIC X(4)    VALUE HIGH-VALUE.           
020200                                                                          
020300     03  W-WDB2ASEQ-LOW.                                                  
020400         05  W-IDPARTNR-B2-S-LOW  PIC X(9)    VALUE SPACE.                
020500         05  W-IDFTG-B2-S-LOW     PIC 9(2)    VALUE ZERO.                 
020600         05  W-IDDISTR-B2-S-LOW   PIC S9(5)   VALUE +0  COMP-3.           
020700         05  W-IDKUNDNR-B2-S-LOW  PIC S9(7)   VALUE +0  COMP-3.           
020800     03  W-WDB2ASEQ-HIGH.                                                 
020900         05  W-IDPARTNR-B2-S-HIGH PIC X(9)    VALUE SPACE.                
021000         05  W-IDFTG-B2-S-HIGH    PIC 9(2)    VALUE ZERO.                 
021100         05  FILLER               PIC X(7)    VALUE HIGH-VALUE.           
021200     EJECT                                                                
021300*    --- STATUS-KOD FRÅN IMS                                              
021400 01  STATUS-WS                   PIC XX.                                  
021500     88  SEGMENT-FINNS                       VALUE '  '.                  
021600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
021800     SKIP2                                                                
021900 01  GODK-STATUSKODER.                                                    
022000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022100     SKIP3                                                                
022200 01  SSA1                        PIC X(128).                              
022300 01  SSA2                        PIC X(128).                              
022400     EJECT                                                                
022500*    --- IMS FUNKTIONSKODER                                               
022600*01  -COPY W0003                                                          
022700     EJECT                                                                
022800*    ---  DLI INPUT-OUTPUT AREA                                           
022900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
023000     SKIP3                                                                
023100 01  DLI-IO-AREA.                                                         
023200     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
023300     SKIP3                                                                
023400     03  WDB101 REDEFINES IO-AREA.                                        
023500*        05  -COPY WDB101                                                 
023600     EJECT                                                                
023700     03  WLGMTS01 REDEFINES IO-AREA.                                      
023800*        05  -COPY WDB2A1                                                 
023900 01  FILLER                       PIC X(16) VALUE 'DLI-IO-WDB1B1'.        
024000 01  DLI-IO-WDB1B1.                                                       
024100*    03  -COPY WDB1B1                                                     
024200                                                                          
024300 01  FILLER                       PIC X(16) VALUE 'DLI-IO-WDB201'.        
024400 01  DLI-IO-WDB201.                                                       
024500*    03  -COPY WDB201                                                     
024600                                                                          
024700     EJECT                                                                
024800 LINKAGE SECTION.                                                         
024900                                                                          
025000*01  -COPY W0009      -PRE MSG-                                           
025100     EJECT                                                                
025200*01  -COPY W0008      -PRE WDP7-                                          
025300     05  FILLER                  PIC X(9).                                
025400     EJECT                                                                
025500*01  -COPY W0008      -PRE WDB1-                                          
025600     05  FILLER                  PIC X(9).                                
025700     EJECT                                                                
025800*01  -COPY W0008      -PRE GMTA-                                          
025900     05  FILLER                  PIC X(10).                               
026000     EJECT                                                                
026100*01  -COPY W0008      -PRE GMTASEQ-                                       
026200     05  FILLER                  PIC X(16).                               
026300*01  -COPY W0008      -PRE WDB1B-                                         
026400     05  FILLER                  PIC X(16).                               
026500     EJECT                                                                
026600 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB                               
026700                     WDB1-PCB GMTA-PCB GMTASEQ-PCB WDB1B-PCB.             
026800     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB                               
026900                     WDB1-PCB GMTA-PCB GMTASEQ-PCB WDB1B-PCB.             
027000                                                                          
027100     PERFORM IMS-GET-MSG                                                  
027200     IF SEGMENT-FINNS                                                     
027300       PERFORM A-INIT                                                     
027400       PERFORM B-KOLLA-NYCKLAR                                            
027500       IF NYCKLAR-OK AND FLVISA-OK                                        
027600         IF MFS-FIRST                                                     
027700           PERFORM C-FOERSTA-SIDA                                         
027800         ELSE                                                             
027900           IF MFS-NEXT                                                    
028000             PERFORM D-NAESTA-SIDA                                        
028100           ELSE                                                           
028200             PERFORM E-SAMMA-SIDA                                         
028300           END-IF                                                         
028400         END-IF                                                           
028500         IF ALLT-OK                                                       
028600         PERFORM F-LAES-VISA-INFO                                         
028700         END-IF                                                           
028800       END-IF                                                             
028900       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
029000       PERFORM IMS-INSERT-MSG                                             
029100     END-IF                                                               
029200                                                                          
029300     MOVE ZERO TO RETURN-CODE                                             
029400     GOBACK                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 A-INIT SECTION.                                                          
029800                                                                          
029900     IF MSG-DUBBLA-TRANSKODER                                             
030000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I48201                 
030100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
030200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
030300     ELSE                                                                 
030400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I48201                  
030500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
030600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030700     END-IF                                                               
030800                                                                          
030900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
031100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031200                                                                          
031300     MOVE LOW-VALUE TO MSG-AREA                                           
031400     MOVE 'W4O482N1' TO MFS-IDMOD                                         
031500     MOVE '4482' TO MOD-IDTRANS                                           
031600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
031700                                                                          
031800     IF NOT EGEN-MID                                                      
031900       MOVE SPACE TO MFS-KDTRTYP                                          
032000       MOVE '7' TO MFS-IDPFK                                              
032100     END-IF                                                               
032200                                                                          
032300     MOVE 'GB ' TO MED-IDSKYLT                                            
032400     .                                                                    
032500     EJECT                                                                
032600 B-KOLLA-NYCKLAR SECTION.                                                 
032700                                                                          
032800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032900     MOVE '001'             TO MSGI-KDCALL                                
033000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033200     MOVE '4482'            TO MSGI-IDTRANS                               
033300     MOVE MID-FLVISA-IN     TO MSGI-FLVISA                                
033400                                                                          
033500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
033600     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
033601     IF SAVE-IDTRANS = '4482'                                             
033602        IF SAVE-IDFTG-ENTER     NOT NUMERIC                               
033603           MOVE ZERO   TO SAVE-IDFTG-ENTER                                
033604        END-IF                                                            
033605        IF SAVE-IDDISTR-ENTER   NOT NUMERIC                               
033606           MOVE ZERO   TO SAVE-IDDISTR-ENTER                              
033607        END-IF                                                            
033608        IF SAVE-IDKUNDNR-ENTER  NOT NUMERIC                               
033609           MOVE ZERO   TO SAVE-IDKUNDNR-ENTER                             
033610        END-IF                                                            
033611        IF SAVE-IDFTG-NEXT      NOT NUMERIC                               
033612           MOVE ZERO   TO SAVE-IDFTG-NEXT                                 
033613        END-IF                                                            
033614        IF SAVE-IDDISTR-NEXT    NOT NUMERIC                               
033615           MOVE ZERO   TO SAVE-IDDISTR-NEXT                               
033616        END-IF                                                            
033617        IF SAVE-IDKUNDNR-NEXT   NOT NUMERIC                               
033618           MOVE ZERO   TO SAVE-IDKUNDNR-NEXT                              
033619        END-IF                                                            
033620     ELSE                                                                 
033621        MOVE '4482' TO SAVE-IDTRANS                                       
033622        MOVE SPACE  TO SAVE-IDPARTNR-ENTER                                
033623        MOVE ZERO   TO SAVE-IDFTG-ENTER                                   
033624        MOVE SPACE  TO SAVE-IDMARKBO-ENTER                                
033625        MOVE ZERO   TO SAVE-IDDISTR-ENTER                                 
033626        MOVE ZERO   TO SAVE-IDKUNDNR-ENTER                                
033627        MOVE SPACE  TO SAVE-IDPARTNR-NEXT                                 
033628        MOVE ZERO   TO SAVE-IDFTG-NEXT                                    
033629        MOVE SPACE  TO SAVE-IDMARKBO-NEXT                                 
033630        MOVE ZERO   TO SAVE-IDDISTR-NEXT                                  
033631        MOVE ZERO   TO SAVE-IDKUNDNR-NEXT                                 
033640     END-IF                                                               
033700                                                                          
033800     MOVE JA TO NYCKLAR-SW                                                
033900                                                                          
034000     MOVE MFS-RENSA-FAELT   TO MOD-IDPARTNR-IN                            
034100                               MOD-IDFTG-IN                               
034200                               MOD-IDDISTR-IN                             
034300                               MOD-IDKUNDNR-IN                            
034400                               MOD-IDLAND-IN                              
034500                                                                          
034600     IF NOT GODK-MID                                                      
034700       MOVE '++++'          TO MID-IDDISTR-IN                             
034800       MOVE '++++++'        TO MID-IDKUNDNR-IN                            
034900     END-IF                                                               
035000                                                                          
035100     PERFORM BA-FLYTTA-NY-GAMM-NYCKEL                                     
035200     PERFORM BB-KOLLA-BET-GMT-NYCKLAR                                     
035300                                                                          
035400                                                                          
035500     IF GODK-MID OR NYCKLAR-OK OR FLVISA-OK                               
035600       IF BET-KEY                                                         
035700         MOVE WS-IDPARTNR     TO MOD-IDPARTNR-UT                          
035800         MOVE WS-IDFTG        TO MOD-IDFTG-UT                             
035900         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
036000                                 MOD-IDKUNDNR-UT                          
036100                                 MOD-IDLAND-UT                            
036200       ELSE                                                               
036300         IF GMT-KEY                                                       
036400           MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR-UT                        
036500           MOVE MFS-RENSA-FAELT TO MOD-IDFTG-UT                           
036600           MOVE MFS-RENSA-FAELT TO MOD-IDLAND-UT                          
036700           MOVE WS-IDDISTR      TO MOD-IDDISTR-UT                         
036800           MOVE MSGI-FLVISA     TO MOD-FLVISA-UT                          
036900           INSPECT MOD-IDDISTR-UT                                         
037000             REPLACING LEADING ZERO BY SPACE                              
037100           MOVE WS-IDKUNDNR     TO MOD-IDKUNDNR-UT                        
037200           INSPECT MOD-IDKUNDNR-UT                                        
037300             REPLACING LEADING ZERO BY SPACE                              
037400         ELSE                                                             
037500           IF CTR-KEY                                                     
037600             MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR-UT                      
037700                                     MOD-IDFTG-UT                         
037800                                     MOD-IDDISTR-UT                       
037900                                     MOD-IDKUNDNR-UT                      
038000             MOVE WS-IDLANDX2     TO MOD-IDLAND-UT                        
038100                                                                          
038200           ELSE                                                           
038300             MOVE WS-IDPARTNR     TO MOD-IDPARTNR-UT                      
038400             MOVE WS-IDFTG        TO MOD-IDFTG-UT                         
038500             MOVE WS-IDDISTR      TO MOD-IDDISTR-UT                       
038600             INSPECT MOD-IDDISTR-UT                                       
038700               REPLACING LEADING ZERO BY SPACE                            
038800             MOVE WS-IDKUNDNR     TO MOD-IDKUNDNR-UT                      
038900             INSPECT MOD-IDKUNDNR-UT                                      
039000               REPLACING LEADING ZERO BY SPACE                            
039100             MOVE WS-IDKUNDNR     TO MOD-IDLAND-UT                        
039200             MOVE NEJ TO NYCKLAR-SW                                       
039300           END-IF                                                         
039400         END-IF                                                           
039500       END-IF                                                             
039600     ELSE                                                                 
039700       MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR-UT                            
039800                               MOD-IDFTG-UT                               
039900                               MOD-IDDISTR-UT                             
040000                               MOD-IDKUNDNR-UT                            
040100                               MOD-IDLAND-UT                              
040200     END-IF                                                               
040300                                                                          
040400     IF NYCKLAR-FEL                                                       
040500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
040600       CALL WMEDKONV USING MED-WMEDAREA                                   
040700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
040800                                                                          
040900*      PERFORM MFS-RENSA-FAELT-UT                                         
041000       MOVE +1 TO INDX                                                    
041100       PERFORM UNTIL INDX > MAX-INDX                                      
041200         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
041300         ADD +1 TO INDX                                                   
041400       END-PERFORM                                                        
041500     END-IF                                                               
041600     IF FLVISA-FEL                                                        
041700       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
041800       CALL WMEDKONV USING MED-WMEDAREA                                   
041900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
042300 BA-FLYTTA-NY-GAMM-NYCKEL SECTION.                                        
042400                                                                          
042500                                                                          
042600     IF MID-IDPARTNR-IN = ALL '+'                                         
042700       MOVE MID-IDPARTNR-UT TO WS-IDPARTNR                                
042800     ELSE                                                                 
042900       MOVE MID-IDPARTNR-IN TO WS-IDPARTNR                                
043000     END-IF                                                               
043100                                                                          
043200     IF MID-IDFTG-IN = ALL '+'                                            
043300       MOVE MID-IDFTG-UT         TO WS-IDFTG                              
043400       INSPECT WS-IDFTG REPLACING LEADING SPACE BY ZERO                   
043500     ELSE                                                                 
043600      MOVE MID-IDFTG-IN          TO WS-IDFTG                              
043700     END-IF                                                               
043800                                                                          
043900     IF WS-IDFTG = ZERO OR SPACES                                         
044000       MOVE MSGI-IDFTG           TO WS-IDFTG                              
044100     END-IF                                                               
044200                                                                          
044300*    IF MID-FLVISA-IN = ALL '+'                                           
044400*      IF MSGI-FLVISA = 'Y' OR 'N'                                        
044500*       MOVE MSGI-FLVISA          TO WS-FLVISA                            
044600*      ELSE                                                               
044700*       MOVE SPACES TO WS-FLVISA                                          
044800*      END-IF                                                             
044900*    ELSE                                                                 
045000*     MOVE MID-FLVISA-IN         TO WS-FLVISA                             
045100*    END-IF                                                               
045200                                                                          
045300                                                                          
045400     IF MID-IDDISTR-IN = ALL '+'                                          
045500       MOVE MID-IDDISTR-UT TO WS-IDDISTR                                  
045600       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
045700     ELSE                                                                 
045800      MOVE MID-IDDISTR-IN TO WS-IDDISTR                                   
045900     END-IF                                                               
046000                                                                          
046100     IF MID-IDKUNDNR-IN = ALL '+'                                         
046200       MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                                
046300       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
046400     ELSE                                                                 
046500       MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                                
046600     END-IF                                                               
046700                                                                          
046800     IF MID-IDLAND-IN = ALL '+'                                           
046900       MOVE MID-IDLAND-UT TO WS-IDLANDX2                                  
047000     ELSE                                                                 
047100       MOVE MID-IDLAND-IN TO WS-IDLANDX2                                  
047200     END-IF                                                               
047300     .                                                                    
047400     EJECT                                                                
047500 BB-KOLLA-BET-GMT-NYCKLAR SECTION.                                        
047600                                                                          
047700     IF MID-IDPARTNR-IN    NOT  = ALL '+' OR                              
047800        MID-IDFTG-IN       NOT  = ALL '+'                                 
047900       IF MID-IDDISTR-IN     = ALL '+' AND                                
048000         MID-IDKUNDNR-IN     = ALL '+'                                    
048100         MOVE JA            TO NYA-NYCKLAR-SW                             
048200         MOVE '7'           TO MFS-IDPFK                                  
048300         MOVE 'BET'         TO NYCKEL-KEY-SW                              
048400       ELSE                                                               
048500         MOVE NEJ           TO NYCKLAR-SW                                 
048600       END-IF                                                             
048700     ELSE                                                                 
048800       IF MID-IDDISTR-IN NOT = ALL '+' OR                                 
048900         MID-IDKUNDNR-IN NOT = ALL '+'                                    
049000         IF MID-IDPARTNR-IN     = ALL '+' AND                             
049100            MID-IDFTG-IN        = ALL '+'                                 
049200           MOVE JA          TO NYA-NYCKLAR-SW                             
049300           MOVE '7'         TO MFS-IDPFK                                  
049400           MOVE 'GMT'       TO NYCKEL-KEY-SW                              
049500         END-IF                                                           
049600       ELSE                                                               
049700         IF MID-IDLAND-IN NOT = ALL '+'                                   
049800           IF MID-IDPARTNR-IN = ALL '+' AND                               
049900              MID-IDFTG-IN    = ALL '+' AND                               
050000              MID-IDKUNDNR-IN = ALL '+'  AND                              
050100              MID-IDDISTR-IN = ALL '+'                                    
050200             MOVE JA          TO NYA-NYCKLAR-SW                           
050300             MOVE '7'         TO MFS-IDPFK                                
050400             MOVE 'CTR'       TO NYCKEL-KEY-SW                            
050500           ELSE                                                           
050600             MOVE NEJ TO NYCKLAR-SW                                       
050700           END-IF                                                         
050800         ELSE                                                             
050900           IF MID-IDPARTNR-UT NOT   = SPACE AND                           
051000              MID-IDFTG-UT NOT = ZERO                                     
051100             MOVE 'BET'           TO NYCKEL-KEY-SW                        
051200           ELSE                                                           
051300             IF MID-IDLAND-UT NOT = SPACE                                 
051400               MOVE 'CTR'         TO NYCKEL-KEY-SW                        
051500               MOVE MID-IDLAND-UT TO WS-IDLANDX2                          
051600             ELSE                                                         
051700               IF MID-IDDISTR-UT NOT = ZERO AND                           
051800                 MID-IDKUNDNR-UT NOT = ZERO                               
051900                 MOVE 'GMT'         TO NYCKEL-KEY-SW                      
052000               ELSE                                                       
052100                 MOVE NEJ           TO NYCKLAR-SW                         
052200               END-IF                                                     
052300             END-IF                                                       
052400           END-IF                                                         
052500         END-IF                                                           
052600       END-IF                                                             
052700     END-IF                                                               
052800                                                                          
052900     IF BET-KEY                                                           
053000       IF WS-IDPARTNR  = SPACE OR                                         
053100          WS-IDFTG NOT NUMERIC OR                                         
053200          WS-IDFTG = ZERO                                                 
053300         MOVE NEJ    TO NYCKLAR-SW                                        
053400       ELSE                                                               
053500         MOVE WS-IDPARTNR TO W-IDPARTNR-B1-S-LOW                          
053600                             W-IDPARTNR-B1-S-HIGH                         
053700                             W-IDPARTNR-B2-S-LOW                          
053800                             W-IDPARTNR-B2-S-HIGH                         
053900         MOVE WS-IDFTG    TO W-IDFTG-B1-S-LOW                             
054000                             W-IDFTG-B1-S-HIGH                            
054100                             W-IDFTG-B2-S-LOW                             
054200                             W-IDFTG-B2-S-HIGH                            
054300       END-IF                                                             
054400     ELSE                                                                 
054500       IF GMT-KEY                                                         
054600         IF WS-IDDISTR  NOT NUMERIC OR                                    
054700           WS-IDDISTR     = ZERO OR                                       
054800           WS-IDKUNDNR  NOT NUMERIC                                       
054900           MOVE NEJ TO NYCKLAR-SW                                         
055000         ELSE                                                             
055100          IF MSGI-FLVISA NOT = ' ' AND 'Y' AND 'J' AND 'N'                
055200           MOVE NEJ       TO CHK-FLVISA-SW                                
055300           MOVE MFS-ADD-READ-HILIGHT-FIELD                                
055400                                    TO MOD-FLVISA-UT-ATTR                 
055500           CALL WMEDKONV USING MED-WMEDAREA                               
055600           MOVE MED-MFSINF TO MOD-TEMFSFEL                                
055700         END-IF                                                           
055800                                                                          
055900           MOVE WS-IDDISTR  TO W-IDDISTR                                  
056000                               W-IDDISTR-LOW                              
056100                               W-IDDISTR-HIGH                             
056200           MOVE WS-IDKUNDNR TO W-IDKUNDNR-LOW                             
056300         END-IF                                                           
056400       ELSE                                                               
056500         IF CTR-KEY                                                       
056600           IF WS-IDLANDX2 = SPACE                                         
056700             MOVE NEJ TO NYCKLAR-SW                                       
056800           ELSE                                                           
056900             MOVE WS-IDLANDX2 TO W-IDLANDX2-HIGH                          
057000                                 W-IDLANDX2-LOW                           
057100           END-IF                                                         
057200         END-IF                                                           
057300       END-IF                                                             
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700 C-FOERSTA-SIDA SECTION.                                                  
057800                                                                          
057900     IF NYA-NYCKLAR                                                       
058000       CONTINUE                                                           
058100      ELSE                                                                
058200       MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                
058300       CALL WMEDKONV USING MED-WMEDAREA                                   
058400       MOVE MED-MFSINF     TO MOD-TEMFSINF                                
058500     END-IF                                                               
058600     MOVE JA TO ALLT-SW                                                   
058700     .                                                                    
058800     EJECT                                                                
058900 D-NAESTA-SIDA SECTION.                                                   
059000                                                                          
059100     IF EGEN-MID                                                          
059200      IF BET-KEY                                                          
059300         MOVE SAVE-IDPARTNR-NEXT  TO W-IDPARTNR-B1-S-LOW                  
059400         MOVE SAVE-IDFTG-NEXT     TO W-IDFTG-B1-S-LOW                     
059500                                    W-IDFTG-B2-S-LOW                      
059600                                    W-IDFTG-B2-S-HIGH                     
059700         MOVE SAVE-IDDISTR-NEXT  TO W-IDDISTR-B2-S-LOW                    
059800         MOVE SAVE-IDKUNDNR-NEXT TO W-IDKUNDNR-B2-S-LOW                   
059900      ELSE                                                                
060000       IF MID-IDLAND-UT NOT = SPACE                                       
060100                                                                          
060200           MOVE SAVE-IDPARTNR-NEXT TO W-IDPARTNR-LOW                      
060300           MOVE SAVE-IDFTG-NEXT    TO W-IDFTG-LOW                         
060400           MOVE SAVE-IDMARKBO-NEXT TO W-IDMARKBO-LOW                      
060500                                                                          
060600           MOVE SAVE-IDDISTR-NEXT  TO WS-SPAR-IDDISTR                     
060700           MOVE SAVE-IDKUNDNR-NEXT TO WS-SPAR-IDKUNDNR                    
060800           MOVE MID-IDLAND-UT     TO W-IDLANDX2-LOW                       
060900                                     W-IDLANDX2-HIGH                      
061000       ELSE                                                               
061100           IF SAVE-IDKUNDNR-NEXT NUMERIC                                  
061200           MOVE SAVE-IDKUNDNR-NEXT TO W-IDKUNDNR-LOW                      
061300           ELSE                                                           
061400           MOVE ZERO TO W-IDKUNDNR-LOW                                    
061500           END-IF                                                         
061600       END-IF                                                             
061700      END-IF                                                              
061800     END-IF                                                               
061900     MOVE JA TO ALLT-SW                                                   
062000     .                                                                    
062100     EJECT                                                                
062200 E-SAMMA-SIDA SECTION.                                                    
062300                                                                          
062400     IF EGEN-MID                                                          
062500      IF BET-KEY                                                          
062600       MOVE SAVE-IDPARTNR-ENTER  TO W-IDPARTNR-B1-S-LOW                   
062700                                   W-IDPARTNR-B2-S-LOW                    
062800                                   W-IDPARTNR-B2-S-HIGH                   
062900       MOVE SAVE-IDFTG-ENTER     TO W-IDFTG-B1-S-LOW                      
063000                                   W-IDFTG-B2-S-LOW                       
063100                                   W-IDFTG-B2-S-HIGH                      
063200       IF SAVE-IDDISTR-ENTER NUMERIC                                      
063300        MOVE SAVE-IDDISTR-ENTER  TO W-IDDISTR-B2-S-LOW                    
063400       ELSE                                                               
063500        MOVE ZERO TO W-IDDISTR-B2-S-LOW                                   
063600       END-IF                                                             
063700       IF SAVE-IDKUNDNR-ENTER NUMERIC                                     
063800        MOVE SAVE-IDKUNDNR-ENTER TO W-IDKUNDNR-B2-S-LOW                   
063900       ELSE                                                               
064000        MOVE ZERO TO W-IDKUNDNR-B2-S-LOW                                  
064100       END-IF                                                             
064200      ELSE                                                                
064300       IF CTR-KEY                                                         
064400         MOVE SAVE-IDPARTNR-ENTER TO W-IDPARTNR-LOW                       
064500         MOVE SAVE-IDFTG-ENTER    TO W-IDFTG-LOW                          
064600         MOVE SAVE-IDMARKBO-ENTER TO W-IDMARKBO-LOW                       
064700                                                                          
064800         IF SAVE-IDDISTR-ENTER NUMERIC                                    
064900          MOVE SAVE-IDDISTR-ENTER  TO WS-SPAR-IDDISTR                     
065000         ELSE                                                             
065100          MOVE ZERO TO WS-SPAR-IDDISTR                                    
065200         END-IF                                                           
065300         IF SAVE-IDKUNDNR-ENTER NUMERIC                                   
065400          MOVE SAVE-IDKUNDNR-ENTER TO WS-SPAR-IDKUNDNR                    
065500         ELSE                                                             
065600          MOVE ZERO TO WS-SPAR-IDKUNDNR                                   
065700         END-IF                                                           
065800       ELSE                                                               
065900         IF SAVE-IDKUNDNR-ENTER NUMERIC                                   
066000          MOVE SAVE-IDKUNDNR-ENTER TO W-IDKUNDNR-LOW                      
066100         ELSE                                                             
066200          MOVE ZERO TO W-IDKUNDNR-LOW                                     
066300         END-IF                                                           
066400       END-IF                                                             
066500      END-IF                                                              
066600     END-IF                                                               
066700     MOVE JA TO ALLT-SW                                                   
066800     .                                                                    
066900     EJECT                                                                
067000 F-LAES-VISA-INFO SECTION.                                                
067100                                                                          
067200     IF BET-KEY                                                           
067300       PERFORM FA-LAES-VISA-INFO-BET-KEY                                  
067400     ELSE                                                                 
067500       IF GMT-KEY                                                         
067600         PERFORM FB-LAES-VISA-INFO-GMT-KEY                                
067700       ELSE                                                               
067800         IF CTR-KEY                                                       
067900           PERFORM FC-LAES-VISA-INFO-CTR-KEY                              
068000         END-IF                                                           
068100       END-IF                                                             
068200     END-IF                                                               
068300     MOVE '002'      TO MSGI-KDCALL                                       
068400     MOVE '4482'     TO SAVE-IDTRANS                                      
068500     MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                    
068600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
068700     .                                                                    
068800     EJECT                                                                
068900 FA-LAES-VISA-INFO-BET-KEY SECTION.                                       
069000                                                                          
069100     PERFORM IMS-GU-HL-WDB101                                             
069200                                                                          
069300     IF SEGMENT-SAKNAS                                                    
069400       IF MFS-NEXT                                                        
069500         MOVE INF-LAST-PAGE-ALREADY TO MED-IDMFSINF                       
069600         CALL WMEDKONV USING MED-WMEDAREA                                 
069700         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
069800*        PERFORM MFS-ROER-EJ-FAELT-UT                                     
069900         MOVE +1 TO INDX                                                  
070000         PERFORM UNTIL INDX > MAX-INDX                                    
070100           PERFORM MFS-ROER-EJ-RAD-FAELT-UT                               
070200           ADD +1 TO INDX                                                 
070300         END-PERFORM                                                      
070400       ELSE                                                               
070500         MOVE INF-FINAN-CUST-MISSING  TO MED-IDMFSINF                     
070600         CALL WMEDKONV USING MED-WMEDAREA                                 
070700         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
070800*        PERFORM MFS-RENSA-FAELT-UT                                       
070900         MOVE +1 TO INDX                                                  
071000         PERFORM UNTIL INDX > MAX-INDX                                    
071100           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
071200           ADD +1 TO INDX                                                 
071300         END-PERFORM                                                      
071400       END-IF                                                             
071500     ELSE                                                                 
071600       MOVE +1 TO INDX                                                    
071700       PERFORM UNTIL INDX > MAX-INDX                                      
071800         IF SEGMENT-FINNS                                                 
071900           IF INDX = +1                                                   
072000             MOVE BET-IDPARTNR      TO SAVE-IDPARTNR-ENTER                
072100             MOVE BET-IDFTG         TO SAVE-IDFTG-ENTER                   
072200             MOVE BET-IDMARKBO      TO SAVE-IDMARKBO-ENTER                
072300           END-IF                                                         
072400           MOVE BET-IDPARTNR        TO MOD-IDPARTNR(INDX)                 
072500                                       W-IDPARTNR-B2-S-LOW                
072600                                       W-IDPARTNR-B2-S-HIGH               
072700                                       SAVE-IDPARTNR-NEXT                 
072800           MOVE BET-IDFTG           TO W-IDFTG-B2-S-LOW                   
072900                                       W-IDFTG-B2-S-HIGH                  
073000                                       SAVE-IDFTG-NEXT                    
073100           MOVE BET-BEBETRAD-1      TO MOD-BEBET   (INDX)                 
073200           MOVE BET-IDMARKBO        TO SAVE-IDMARKBO-NEXT                 
073300                                                                          
073400                                                                          
073500           PERFORM IMS-GU-WLGMTA01SEQ                                     
073600           MOVE NEJ TO SAMMA-BETALARE-SW                                  
073700                       GMT-SAKNAS-SW                                      
073800                                                                          
073900           PERFORM UNTIL INDX > MAX-INDX OR GMT-SAKNAS                    
074000             IF SEGMENT-FINNS                                             
074100               IF INDX = +1                                               
074200                 MOVE GMT-IDDISTR  TO SAVE-IDDISTR-ENTER                  
074300                 MOVE GMT-IDKUNDNR TO SAVE-IDKUNDNR-ENTER                 
074400               END-IF                                                     
074500                                                                          
074600               IF SAMMA-BETALARE                                          
074700                 MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR (INDX)              
074800                                         MOD-BEBET (INDX)                 
074900               ELSE                                                       
075000                 MOVE JA TO SAMMA-BETALARE-SW                             
075100               END-IF                                                     
075200                                                                          
075300               MOVE GMT-IDDISTR    TO MOD-IDDISTR (INDX)                  
075400               MOVE GMT-IDKUNDNR   TO MOD-IDKUNDNR (INDX)                 
075500               MOVE GMT-BEGMT-RAD1 TO MOD-BEGODSM (INDX)                  
075600                                                                          
075700               ADD +1 TO INDX                                             
075800               PERFORM IMS-GN-WLGMTA01SEQ                                 
075900                                                                          
076000             ELSE                                                         
076100               IF NY-BETALARE                                             
076200                 MOVE MFS-RENSA-FAELT TO MOD-IDDISTR (INDX)               
076300                                         MOD-IDKUNDNR (INDX)              
076400                                         MOD-BEGODSM (INDX)               
076500                 ADD +1 TO INDX                                           
076600               END-IF                                                     
076700               MOVE JA TO GMT-SAKNAS-SW                                   
076800             END-IF                                                       
076900           END-PERFORM                                                    
077000           IF INDX > MAX-INDX                                             
077100             IF SEGMENT-FINNS                                             
077200               MOVE GMT-IDDISTR     TO SAVE-IDDISTR-NEXT                  
077300               MOVE GMT-IDKUNDNR    TO SAVE-IDKUNDNR-NEXT                 
077400             ELSE                                                         
077500               PERFORM IMS-GN-HL-WDB101                                   
077600               IF SEGMENT-FINNS                                           
077700                 MOVE BET-IDPARTNR     TO SAVE-IDPARTNR-NEXT              
077800                 MOVE BET-IDFTG        TO SAVE-IDFTG-NEXT                 
077900                 MOVE BET-IDMARKBO     TO SAVE-IDMARKBO-NEXT              
078000               END-IF                                                     
078100               MOVE ZERO               TO SAVE-IDDISTR-ENTER              
078200                                          SAVE-IDDISTR-NEXT               
078300                                          SAVE-IDKUNDNR-ENTER             
078400                                          SAVE-IDKUNDNR-NEXT              
078500             END-IF                                                       
078600           ELSE                                                           
078700             PERFORM IMS-GN-HL-WDB101                                     
078800           END-IF                                                         
078900                                                                          
079000         ELSE                                                             
079100           PERFORM UNTIL INDX > MAX-INDX                                  
079200             PERFORM MFS-RENSA-RAD-FAELT-UT                               
079300             ADD +1 TO INDX                                               
079400           END-PERFORM                                                    
079500         END-IF                                                           
079600       END-PERFORM                                                        
079700                                                                          
079800       IF SEGMENT-FINNS                                                   
079900         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
080000         CALL WMEDKONV USING MED-WMEDAREA                                 
080100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
080200       ELSE                                                               
080300         MOVE INF-LAST-PAGE-ALREADY TO MED-IDMFSINF                       
080400         CALL WMEDKONV USING MED-WMEDAREA                                 
080500         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
080600       END-IF                                                             
080700                                                                          
080800     END-IF                                                               
080900     .                                                                    
081000     EJECT                                                                
081100 FB-LAES-VISA-INFO-GMT-KEY SECTION.                                       
081200                                                                          
081300     PERFORM IMS-GU-WLGMTA01                                              
081400                                                                          
081500     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
081600       IF MFS-NEXT                                                        
081700         MOVE INF-LAST-PAGE-ALREADY TO MED-IDMFSINF                       
081800         CALL WMEDKONV USING MED-WMEDAREA                                 
081900         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
082000*        PERFORM MFS-ROER-EJ-FAELT-UT                                     
082100         MOVE +1 TO INDX                                                  
082200         PERFORM UNTIL INDX > MAX-INDX                                    
082300           PERFORM MFS-ROER-EJ-RAD-FAELT-UT                               
082400           ADD +1 TO INDX                                                 
082500         END-PERFORM                                                      
082600       ELSE                                                               
082700         MOVE INF-RECIEVER-MISSING  TO MED-IDMFSINF                       
082800         CALL WMEDKONV USING MED-WMEDAREA                                 
082900         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
083000*        PERFORM MFS-RENSA-FAELT-UT                                       
083100         MOVE +1 TO INDX                                                  
083200         PERFORM UNTIL INDX > MAX-INDX                                    
083300           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
083400           ADD +1 TO INDX                                                 
083500         END-PERFORM                                                      
083600       END-IF                                                             
083700     ELSE                                                                 
083800       MOVE +1 TO INDX                                                    
083900       PERFORM UNTIL INDX > MAX-INDX                                      
084000         IF SEGMENT-FINNS                                                 
084100          IF INDX = +1                                                    
084200             MOVE GMT-IDDISTR  TO SAVE-IDDISTR-ENTER                      
084300             MOVE GMT-IDKUNDNR TO SAVE-IDKUNDNR-ENTER                     
084400          END-IF                                                          
084500          IF (MSGI-FLVISA = 'Y' OR MSGI-FLVISA = 'J') AND                 
084600              GMT-TISTODAT > 0                                            
084700           CONTINUE                                                       
084800          ELSE                                                            
084900           MOVE GMT-IDDISTR    TO MOD-IDDISTR (INDX)                      
085000           MOVE GMT-IDKUNDNR   TO MOD-IDKUNDNR (INDX)                     
085100           MOVE GMT-BEGMT-RAD1 TO MOD-BEGODSM (INDX)                      
085200                                                                          
085300           MOVE GMT-IDPARTNR   TO W-WDB1-IDPARTNR                         
085400           MOVE GMT-IDFTG      TO W-WDB1-IDFTG                            
085500                                                                          
085600           PERFORM IMS-GU-WDB101                                          
085700                                                                          
085800           IF SEGMENT-FINNS                                               
085900             MOVE BET-IDPARTNR    TO MOD-IDPARTNR(INDX)                   
086000             MOVE BET-BEBETRAD-1  TO MOD-BEBET   (INDX)                   
086100           ELSE                                                           
086200             MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR(INDX)                   
086300                                     MOD-BEBET   (INDX)                   
086400           END-IF                                                         
086500           ADD +1 TO INDX                                                 
086600          END-IF                                                          
086700          PERFORM IMS-GN-WLGMTA01                                         
086800         ELSE                                                             
086900           PERFORM UNTIL INDX > MAX-INDX                                  
087000             PERFORM MFS-RENSA-RAD-FAELT-UT                               
087100             ADD +1 TO INDX                                               
087200           END-PERFORM                                                    
087300         END-IF                                                           
087400       END-PERFORM                                                        
087500                                                                          
087600       IF SEGMENT-FINNS                                                   
087700         MOVE GMT-IDKUNDNR     TO SAVE-IDKUNDNR-NEXT                      
087800         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
087900         CALL WMEDKONV USING MED-WMEDAREA                                 
088000         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
088100       ELSE                                                               
088200         MOVE INF-LAST-PAGE-ALREADY TO MED-IDMFSINF                       
088300         CALL WMEDKONV USING MED-WMEDAREA                                 
088400         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
088500       END-IF                                                             
088600                                                                          
088700     END-IF                                                               
088800     .                                                                    
088900     EJECT                                                                
089000 FC-LAES-VISA-INFO-CTR-KEY SECTION.                                       
089100                                                                          
089200     PERFORM IMS-GU-WDB1B1KY                                              
089300     IF SEGMENT-FINNS                                                     
089400       MOVE SEQB-IDPARTNR TO W-WDB1-IDPARTNR                              
089500       MOVE SEQB-IDFTG    TO W-WDB1-IDFTG                                 
089600       PERFORM IMS-GU-WDB101                                              
089700     END-IF                                                               
089800                                                                          
089900     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
090000       IF MFS-NEXT                                                        
090100         MOVE INF-LAST-PAGE-ALREADY TO MED-IDMFSINF                       
090200         CALL WMEDKONV USING MED-WMEDAREA                                 
090300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
090400*        PERFORM MFS-ROER-EJ-FAELT-UT                                     
090500         MOVE +1 TO INDX                                                  
090600         PERFORM UNTIL INDX > MAX-INDX                                    
090700           PERFORM MFS-ROER-EJ-RAD-FAELT-UT                               
090800           ADD +1 TO INDX                                                 
090900         END-PERFORM                                                      
091000       ELSE                                                               
091100         MOVE INF-RECIEVER-MISSING  TO MED-IDMFSINF                       
091200         CALL WMEDKONV USING MED-WMEDAREA                                 
091300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
091400*        PERFORM MFS-RENSA-FAELT-UT                                       
091500         MOVE +1 TO INDX                                                  
091600         PERFORM UNTIL INDX > MAX-INDX                                    
091700           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
091800           ADD +1 TO INDX                                                 
091900         END-PERFORM                                                      
092000       END-IF                                                             
092100     ELSE                                                                 
092200       MOVE +1 TO INDX                                                    
092300       PERFORM UNTIL INDX > MAX-INDX                                      
092400         IF MFS-NEXT OR MFS-ENTER                                         
092500           PERFORM FCA-LAES-FRAM                                          
092600         ELSE                                                             
092700           IF INDX = +1                                                   
092800             MOVE BET-IDPARTNR   TO SAVE-IDPARTNR-ENTER                   
092900             MOVE BET-IDFTG      TO SAVE-IDFTG-ENTER                      
093000             MOVE BET-IDMARKBO   TO SAVE-IDMARKBO-ENTER                   
093100           END-IF                                                         
093200           MOVE BET-IDPARTNR     TO  SAVE-IDPARTNR-NEXT                   
093300                                      MOD-IDPARTNR(INDX)                  
093400                                      W-IDPARTNR-B2-S-LOW                 
093500                                      W-IDPARTNR-B2-S-HIGH                
093600           MOVE BET-IDFTG        TO  SAVE-IDFTG-NEXT                      
093700                                      W-IDFTG-B2-S-LOW                    
093800                                      W-IDFTG-B2-S-HIGH                   
093900           MOVE BET-IDMARKBO     TO  SAVE-IDMARKBO-NEXT                   
094000           MOVE BET-BEBETRAD-1   TO MOD-BEBET   (INDX)                    
094100                                                                          
094200           PERFORM IMS-GU-WLGMTA01SEQ                                     
094300           IF SEGMENT-SAKNAS                                              
094400             ADD +1 TO INDX                                               
094500           END-IF                                                         
094600         END-IF                                                           
094700                                                                          
094800           MOVE NEJ TO SAMMA-BETALARE-SW                                  
094900                       CTR-SAKNAS-SW                                      
095000                                                                          
095100           PERFORM UNTIL INDX > MAX-INDX OR CTR-SAKNAS                    
095200             IF SEGMENT-FINNS                                             
095300               IF INDX = +1                                               
095400                 MOVE GMT-IDDISTR  TO SAVE-IDDISTR-ENTER                  
095500                 MOVE GMT-IDKUNDNR TO SAVE-IDKUNDNR-ENTER                 
095600               END-IF                                                     
095700                                                                          
095800               IF SAMMA-BETALARE                                          
095900                 MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR (INDX)              
096000                                         MOD-BEBET (INDX)                 
096100               ELSE                                                       
096200                 MOVE JA TO SAMMA-BETALARE-SW                             
096300               END-IF                                                     
096400                                                                          
096500               MOVE GMT-IDDISTR    TO MOD-IDDISTR (INDX)                  
096600               MOVE GMT-IDKUNDNR   TO MOD-IDKUNDNR (INDX)                 
096700               MOVE GMT-BEGMT-RAD1 TO MOD-BEGODSM (INDX)                  
096800                                                                          
096900               ADD +1 TO INDX                                             
097000               PERFORM IMS-GN-WLGMTA01SEQ                                 
097100                                                                          
097200             ELSE                                                         
097300               PERFORM IMS-GN-WDB1B1KY                                    
097400               IF SEGMENT-FINNS                                           
097500                 MOVE SEQB-IDPARTNR TO W-WDB1-IDPARTNR                    
097600                 MOVE SEQB-IDFTG    TO W-WDB1-IDFTG                       
097700                 PERFORM IMS-GU-WDB101                                    
097800                   IF SEGMENT-FINNS                                       
097900                     MOVE BET-IDPARTNR TO W-IDPARTNR-B2-S-LOW             
098000                                              W-IDPARTNR-B2-S-HIGH        
098100                                              MOD-IDPARTNR(INDX)          
098200                     MOVE BET-IDFTG TO W-IDFTG-B2-S-LOW                   
098300                                              W-IDFTG-B2-S-HIGH           
098400                     MOVE BET-BEBETRAD-1  TO MOD-BEBET(INDX)              
098500                     PERFORM IMS-GU-WLGMTA01SEQ                           
098600                     IF SEGMENT-FINNS                                     
098700                       MOVE NEJ TO SAMMA-BETALARE-SW                      
098800                     ELSE                                                 
098900                       ADD +1 TO INDX                                     
099000                     END-IF                                               
099100                   ELSE                                                   
099200                     MOVE JA TO CTR-SAKNAS-SW                             
099300                   END-IF                                                 
099400               ELSE                                                       
099500                 MOVE JA TO CTR-SAKNAS-SW                                 
099600               END-IF                                                     
099700             END-IF                                                       
099800           END-PERFORM                                                    
099900           IF INDX > MAX-INDX                                             
100000             IF SEGMENT-FINNS                                             
100100               MOVE GMT-IDDISTR        TO SAVE-IDDISTR-NEXT               
100200               MOVE GMT-IDKUNDNR       TO SAVE-IDKUNDNR-NEXT              
100300               MOVE SEQB-IDPARTNR      TO SAVE-IDPARTNR-NEXT              
100400               MOVE SEQB-IDFTG         TO SAVE-IDFTG-NEXT                 
100500               MOVE SEQB-IDMARKBO      TO SAVE-IDMARKBO-NEXT              
100600             ELSE                                                         
100700               PERFORM IMS-GN-WDB1B1KY                                    
100800                 MOVE SEQB-IDPARTNR TO W-WDB1-IDPARTNR                    
100900                 MOVE SEQB-IDFTG    TO W-WDB1-IDFTG                       
101000                                                                          
101100               PERFORM IMS-GU-WDB101                                      
101200               IF SEGMENT-FINNS                                           
101300                 MOVE BET-IDPARTNR      TO SAVE-IDPARTNR-NEXT             
101400                                           W-IDPARTNR-B2-S-LOW            
101500                                           W-IDPARTNR-B2-S-HIGH           
101600                 MOVE BET-IDFTG         TO SAVE-IDFTG-NEXT                
101700                                           W-IDFTG-B2-S-LOW               
101800                                           W-IDFTG-B2-S-HIGH              
                       MOVE BET-IDMARKBO     TO  SAVE-IDMARKBO-NEXT             
101900                 MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                
102000                 CALL WMEDKONV USING MED-WMEDAREA                         
102100                 MOVE MED-TEMFSINF TO MOD-TEMFSINF                        
102200                                                                          
102300                 PERFORM IMS-GU-WLGMTA01SEQ                               
102400                 IF SEGMENT-FINNS                                         
102500                   MOVE GMT-IDDISTR  TO SAVE-IDDISTR-NEXT                 
102600                   MOVE GMT-IDKUNDNR TO SAVE-IDKUNDNR-NEXT                
102700                 ELSE                                                     
102800                   MOVE ZERO          TO SAVE-IDDISTR-NEXT                
102900                                         SAVE-IDKUNDNR-NEXT               
103000                 END-IF                                                   
103100               ELSE                                                       
103200                 MOVE HIGH-VALUE       TO SAVE-IDMARKBO-NEXT              
103300                 MOVE ZERO             TO SAVE-IDDISTR-ENTER              
103400                                          SAVE-IDDISTR-NEXT               
103500                                          SAVE-IDKUNDNR-ENTER             
103600                                          SAVE-IDKUNDNR-NEXT              
103700               END-IF                                                     
103800             END-IF                                                       
103900           END-IF                                                         
104000         IF INDX <=  MAX-INDX                                             
104100           PERFORM UNTIL INDX > MAX-INDX                                  
104200             PERFORM MFS-RENSA-RAD-FAELT-UT                               
104300             ADD +1 TO INDX                                               
104400           END-PERFORM                                                    
104500           MOVE INF-LAST-PAGE-ALREADY TO MED-IDMFSINF                     
104600           CALL WMEDKONV USING MED-WMEDAREA                               
104700           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
104800         END-IF                                                           
104900       END-PERFORM                                                        
105000       IF SEGMENT-FINNS                                                   
105100         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
105200         CALL WMEDKONV USING MED-WMEDAREA                                 
105300         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
105400       END-IF                                                             
105500     END-IF                                                               
105600     .                                                                    
105700     EJECT                                                                
105800 FCA-LAES-FRAM SECTION.                                                   
105900     MOVE NEJ TO POST-FINNS-SW                                            
106000     IF SEGMENT-FINNS                                                     
106100*      PERFORM UNTIL WS-SPAR-IDPARTNR = BET-IDPARTNR                      
106200*        PERFORM IMS-GN-WDB1B1KY                                          
106300*      END-PERFORM                                                        
106400                                                                          
106500       MOVE      BET-IDPARTNR  TO SAVE-IDPARTNR-ENTER                     
106600                                  SAVE-IDPARTNR-NEXT                      
106700                                  MOD-IDPARTNR(INDX)                      
106800                                  W-IDPARTNR-B2-S-LOW                     
106900                                  W-IDPARTNR-B2-S-HIGH                    
107000       MOVE      BET-IDFTG     TO SAVE-IDFTG-ENTER                        
107100                                  SAVE-IDFTG-NEXT                         
107200                                  W-IDFTG-B2-S-LOW                        
107300                                  W-IDFTG-B2-S-HIGH                       
107400       MOVE      BET-IDMARKBO  TO SAVE-IDMARKBO-ENTER                     
107500                                  SAVE-IDMARKBO-NEXT                      
107600       MOVE      BET-BEBETRAD-1 TO MOD-BEBET (INDX)                       
107700       PERFORM IMS-GU-WLGMTA01SEQ                                         
107800       IF WS-SPAR-IDDISTR  NOT = ZERO                                     
107900         IF SEGMENT-FINNS                                                 
108000           PERFORM UNTIL POST-FINNS OR SEGMENT-SAKNAS                     
108100             IF WS-SPAR-IDDISTR  = GMT-IDDISTR AND                        
108200                WS-SPAR-IDKUNDNR = GMT-IDKUNDNR                           
108300               MOVE JA TO POST-FINNS-SW                                   
108400             ELSE                                                         
108500               PERFORM IMS-GN-WLGMTA01SEQ                                 
108600             END-IF                                                       
108700           END-PERFORM                                                    
108800         END-IF                                                           
108900       ELSE                                                               
109000         IF SEGMENT-SAKNAS                                                
109100           MOVE ZERO          TO SAVE-IDDISTR-ENTER                       
109200                                 SAVE-IDKUNDNR-ENTER                      
109300           ADD +1 TO INDX                                                 
109400         END-IF                                                           
109500       END-IF                                                             
109600                                                                          
109700     END-IF                                                               
109800     .                                                                    
109900     EJECT                                                                
110000*MFS-RENSA-FAELT-UT SECTION.                                              
110100*                                                                         
110200*                                                                         
110300*    MOVE MFS-RENSA-FAELT TO SAVE-IDPARTNR-ENTER                          
110400*                            SAVE-IDFTG-ENTER                             
110500*                            SAVE-IDMARKBO-ENTER                          
110600*                            SAVE-IDDISTR-ENTER                           
110700*                            SAVE-IDKUNDNR-ENTER                          
110800*                                                                         
110900*                                                                         
111000*                            SAVE-IDPARTNR-NEXT                           
111100*                            SAVE-IDFTG-NEXT                              
111200*                            SAVE-IDMARKBO-NEXT                           
111300*                            SAVE-IDDISTR-NEXT                            
111400*                            SAVE-IDKUNDNR-NEXT                           
111500*    .                                                                    
111600*    EJECT                                                                
111700 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
111800                                                                          
111900                                                                          
112000     MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR (INDX)                          
112100                             MOD-BEBET    (INDX)                          
112200                             MOD-IDDISTR (INDX)                           
112300                             MOD-IDKUNDNR (INDX)                          
112400                             MOD-BEGODSM (INDX)                           
112500     .                                                                    
112600     EJECT                                                                
112700*MFS-ROER-EJ-FAELT-UT SECTION.                                            
112800*                                                                         
112900*                                                                         
113000*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDPARTNR-ENTER                         
113100*                              MOD-IDFTG-ENTER                            
113200*                              MOD-IDMARKBO-ENTER                         
113300*                              MOD-IDDISTR-ENTER                          
113400*                              MOD-IDKUNDNR-ENTER                         
113500*                                                                         
113600*                              MOD-IDPARTNR-NEXT                          
113700*                              MOD-IDFTG-NEXT                             
113800*                              MOD-IDMARKBO-NEXT                          
113900*                              MOD-IDDISTR-NEXT                           
114000*                              MOD-IDKUNDNR-NEXT                          
114100*    .                                                                    
114200*    EJECT                                                                
114300 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
114400                                                                          
114500                                                                          
114600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPARTNR (INDX)                        
114700                               MOD-BEBET (INDX)                           
114800                               MOD-IDDISTR (INDX)                         
114900                               MOD-IDKUNDNR (INDX)                        
115000                               MOD-BEGODSM (INDX)                         
115100     .                                                                    
115200     EJECT                                                                
115300* --- IMS SEKTIONER ---                                                   
115400     SKIP3                                                                
115500 IMS-GET-MSG SECTION.                                                     
115600                                                                          
115700     MOVE '  QC' TO GODK-STATUSKODER                                      
115800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
115900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
116000     PERFORM IMS-STATUSKONTROLL                                           
116100     .                                                                    
116200     SKIP3                                                                
116300 IMS-INSERT-MSG SECTION.                                                  
116400                                                                          
116500     IF ENGLISH-TEXT                                                      
116600       MOVE 'N' TO MFS-KDHUVOMR                                           
116700     END-IF                                                               
116800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
116900     MOVE SPACE TO GODK-STATUSKODER                                       
117000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
117100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
117200     PERFORM IMS-STATUSKONTROLL                                           
117300     .                                                                    
117400     EJECT                                                                
117500 IMS-GU-WDB101 SECTION.                                                   
117600     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
117700          DELIMITED BY SIZE INTO SSA1                                     
117800     MOVE '  GE' TO GODK-STATUSKODER                                      
117900     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA SSA1                      
118000     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
118100     PERFORM IMS-STATUSKONTROLL                                           
118200     .                                                                    
118300     SKIP3                                                                
118400 IMS-GU-HL-WDB101 SECTION.                                                
118500     STRING 'WDB101  (WDB101KY=>' W-WDB1KEY-LOW                           
118600                    '&IDFTG   = ' W-IDFTG-B1-S-LOW ')'                    
118700          DELIMITED BY SIZE INTO SSA1                                     
118800     MOVE '  GE' TO GODK-STATUSKODER                                      
118900     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA SSA1                      
119000     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
119100     PERFORM IMS-STATUSKONTROLL                                           
119200     .                                                                    
119300     SKIP3                                                                
119400 IMS-GN-HL-WDB101 SECTION.                                                
119500     STRING 'WDB101  (WDB101KY=>' W-WDB1KEY-LOW                           
119600                    '&IDFTG   = ' W-IDFTG-B1-S-LOW ')'                    
119700          DELIMITED BY SIZE INTO SSA1                                     
119800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
119900     CALL CBLTDLI USING GN WDB1-PCB DLI-IO-AREA SSA1                      
120000     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
120100     PERFORM IMS-STATUSKONTROLL                                           
120200     .                                                                    
120300     EJECT                                                                
120400 IMS-GU-WLGMTA01 SECTION.                                                 
120500     STRING 'WLGMTA01(IDGMT   =>' W-WDB2KEY-LOW                           
120600                    '&IDGMT    <' W-WDB2KEY-HIGH ')'                      
120700          DELIMITED BY SIZE INTO SSA1                                     
120800     MOVE '  GE' TO GODK-STATUSKODER                                      
120900     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WDB201 SSA1                    
121000     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
121100     PERFORM IMS-STATUSKONTROLL                                           
121200     .                                                                    
121300     SKIP3                                                                
121400 IMS-GN-WLGMTA01 SECTION.                                                 
121500     STRING 'WLGMTA01(IDGMT   =>' W-WDB2KEY-LOW                           
121600                    '&IDGMT    <' W-WDB2KEY-HIGH ')'                      
121700          DELIMITED BY SIZE INTO SSA1                                     
121800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
121900     CALL CBLTDLI USING GN GMTA-PCB DLI-IO-WDB201 SSA1                    
122000     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
122100     PERFORM IMS-STATUSKONTROLL                                           
122200     .                                                                    
122300     SKIP3                                                                
122400 IMS-GU-WLGMTA01SEQ SECTION.                                              
122500     STRING 'WLGMTA01(WDB2ASEQ=>' W-WDB2ASEQ-LOW                          
122600                    '&WDB2ASEQ <' W-WDB2ASEQ-HIGH ')'                     
122700          DELIMITED BY SIZE INTO SSA1                                     
122800     MOVE '  GE' TO GODK-STATUSKODER                                      
122900     CALL CBLTDLI USING GU GMTASEQ-PCB DLI-IO-WDB201 SSA1                 
123000     MOVE GMTASEQ-STATUS-CODE TO STATUS-WS                                
123100     PERFORM IMS-STATUSKONTROLL                                           
123200     .                                                                    
123300     SKIP3                                                                
123400 IMS-GN-WLGMTA01SEQ SECTION.                                              
123500     STRING 'WLGMTA01(WDB2ASEQ=>' W-WDB2ASEQ-LOW                          
123600                    '&WDB2ASEQ <' W-WDB2ASEQ-HIGH ')'                     
123700          DELIMITED BY SIZE INTO SSA1                                     
123800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
123900     CALL CBLTDLI USING GN GMTASEQ-PCB DLI-IO-WDB201 SSA1                 
124000     MOVE GMTASEQ-STATUS-CODE TO STATUS-WS                                
124100     PERFORM IMS-STATUSKONTROLL                                           
124200     .                                                                    
124300     EJECT                                                                
124400 IMS-GU-WDB1B1KY SECTION.                                                 
124500     STRING 'WDB1B1  (WDB1B1KY=>' W-WDB1B1KY-LOW                          
124600                    '&WDB1B1KY=<' W-WDB1B1KY-HIGH ')'                     
124700          DELIMITED BY SIZE INTO SSA1                                     
124800     MOVE '  GE' TO GODK-STATUSKODER                                      
124900     CALL CBLTDLI USING GU WDB1B-PCB DLI-IO-WDB1B1 SSA1                   
125000     MOVE WDB1B-STATUS-CODE TO STATUS-WS                                  
125100     PERFORM IMS-STATUSKONTROLL                                           
125200     .                                                                    
125300     SKIP3                                                                
125400 IMS-GN-WDB1B1KY SECTION.                                                 
125500     STRING 'WDB1B1  (WDB1B1KY=>' W-WDB1B1KY-LOW                          
125600                    '&WDB1B1KY=<' W-WDB1B1KY-HIGH ')'                     
125700          DELIMITED BY SIZE INTO SSA1                                     
125800     MOVE '  GE' TO GODK-STATUSKODER                                      
125900     CALL CBLTDLI USING GN WDB1B-PCB DLI-IO-WDB1B1 SSA1                   
126000     MOVE WDB1B-STATUS-CODE TO STATUS-WS                                  
126100     PERFORM IMS-STATUSKONTROLL                                           
126200     .                                                                    
126300     SKIP3                                                                
126400 IMS-STATUSKONTROLL SECTION.                                              
126500                                                                          
126600     SET STATUS-IX TO 1                                                   
126700     SEARCH GODK-STATUS                                                   
126800       AT END                                                             
126900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
127000         DELIMITED BY SIZE INTO FELTEXT                                   
127100         CALL FELLOG                                                      
127200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
127300     END-SEARCH                                                           
127400     .                                                                    
