000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4041100.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   96/04/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KUNDREGISTER.GODSMOTTAGARE INFORMATION 1.                        
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDB2                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T411                                              
001400*                     W4T411U                                             
001500*                     W4T411V                                             
001600*        MID:         W4I41101                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O41101                                            
002000*                                                                         
002100*    E-TRACKER: 2913019 DATE 20060210                                     
002200*                                                                         
002300*    E-TRACKER: 7450328  HÖST -08   VOHF                                  
002400*                                                                         
002500                                                                          
002600*    E-TRACKER: 8081720  ADD FLVOHF-KL(*) FOR EACH ORDER CLASS            
002700*                                                                         
002710*    E-TRACKER: 10254592 2015       DECOMISSION VOHF                      
002720*                                                                         
002730*    STORY 2350411/RDM 4411 CHANGES/MAKE PARTNER ID/CODE PROTECTED        
002731*                                                                         
002740*    STORY 3101308/MAKE PARTNER ID IN LINE 5 AS EDITABLE                  
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W4041100'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 01  ALL-PLUS.                                                            
004400     03  FILLER                  PIC X(80)   VALUE ALL '+'.               
004500 01  ALL-SPACE.                                                           
004600     03  FILLER                  PIC X(80)   VALUE ALL SPACE.             
004610 01  WS-KDKUNDKAT-REC.                                                    
004620     03  WS-KDKUNDKAT            PIC X(10)   VALUE ALL SPACE.             
004700                                                                          
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005000                                                                          
005100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005200     88  EGEN-MID                            VALUE '4411'.                
005300     88  GODK-MID                            VALUE '4411' '4412'          
005400                                                   '4413' '4414'          
005500                                                   '4415' '4416'          
005600                                                   '4417' '4418'          
005700                                                   '4419'.                
005800     88  HELP-MID                            VALUE '0551'.                
005900                                                                          
006000                                                                          
006100     EJECT                                                                
006200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006300 01  GENERELLA-SUBPROGRAM.                                                
006400     03  W4041110                PIC X(8)    VALUE 'W4041110'.            
006500     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     EJECT                                                                
007000 01  MESSAGE-CODES.                                                       
007100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007600     EJECT                                                                
008010*    --- PARAMETRAR TILL SUBPROGRAM WL01MCNV                              
008020*                                                                         
008030*01 -COPY WL01MCNV                                                        
008040     SKIP3                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400     SKIP3                                                                
008500*01 -COPY WMSGINIT                                                        
008600     SKIP3                                                                
008700 01  FILLER                      PIC X(16)  VALUE 'W4041110'.             
008800 01  REQU-AREA.                                                           
008900*    03 -COPY WZ01REQU                                                    
009000*    03 -COPY W40411I1                                                    
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
009300 01  RESP-AREA.                                                           
009400*    03 -COPY WZ01RESP                                                    
009500*    03 -COPY W40411O1                                                    
009600     EJECT                                                                
009700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010000     SKIP3                                                                
010100*01  MID -COPY W4I41101                                                   
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010400     SKIP3                                                                
010500*01  -COPY WMSGAREA                                                       
010600     EJECT                                                                
010700     03  MOD REDEFINES MSG-AREA.                                          
010800*      05  -COPY W4O41101                                                 
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011100     SKIP3                                                                
011200*01  -COPY WMFSAREA                                                       
011201     EJECT                                                                
011202 01  FILLER                      PIC X(16)  VALUE 'CUSTCAT-AREA'.         
011203     SKIP3                                                                
011230*01 -COPY WWTEXT02                                                        
011300     EJECT                                                                
011400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011500*                                                                         
011600                                                                          
011700*    --- STATUS-KOD FRÅN IMS                                              
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FINNS                       VALUE '  '.                  
012000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012200     SKIP2                                                                
012300 01  GODK-STATUSKODER.                                                    
012400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01  SSA1                        PIC X(64).                               
012700 01  SSA2                        PIC X(64).                               
012800     EJECT                                                                
012900*    --- IMS FUNKTIONSKODER                                               
013000*01  -COPY W0003                                                          
013100     EJECT                                                                
013200                                                                          
013300 LINKAGE SECTION.                                                         
013400*01  -COPY W0009   -PRE MSG-                                              
013500 01  USEA-PCB                    PIC X.                                   
013600 01  WDB2-PCB                    PIC X.                                   
013800     EJECT                                                                
013900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDB2-PCB.                     
014000 MAIN SECTION.                                                            
014100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDB2-PCB.                     
014200                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-INIT-KEYS                                                
014700       PERFORM C-INIT-REQU                                                
014800       IF MFS-UPDATE                                                      
014900         SET REQU-UPDATE         TO TRUE                                  
015000         PERFORM E-SAMMA-SIDA                                             
015100       ELSE                                                               
015200         IF MFS-FIRST                                                     
015300           SET REQU-FIRST        TO TRUE                                  
015400           PERFORM MFS-RENSA-FAELT-IN                                     
015500         ELSE                                                             
015600           SET REQU-QUERY        TO TRUE                                  
015700           PERFORM E-SAMMA-SIDA                                           
015800         END-IF                                                           
015900       END-IF                                                             
016000       PERFORM F-CALL-BIZ-LOGIC-W4041110                                  
016100       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O41101 + 4                      
016200       PERFORM IMS-INSERT-MSG                                             
016300     END-IF                                                               
016400                                                                          
016500     MOVE ZERO TO RETURN-CODE                                             
016600     GOBACK                                                               
016700     .                                                                    
016800     EJECT                                                                
016900 A-INIT SECTION.                                                          
017000                                                                          
017100     IF MSG-DUBBLA-TRANSKODER                                             
017200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I41101                 
017300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017500     ELSE                                                                 
017600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I41101                  
017700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017900     END-IF                                                               
018000                                                                          
018100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018400                                                                          
018500     MOVE LOW-VALUE TO MSG-AREA                                           
018600     MOVE 'W4O411N1' TO MFS-IDMOD                                         
018700     MOVE '4411' TO MOD-IDTRANS                                           
018800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018900                                                                          
019000     IF EGEN-MID OR HELP-MID                                              
019100       CONTINUE                                                           
019200     ELSE                                                                 
019300       MOVE SPACE TO MFS-KDTRTYP                                          
019400       MOVE '7' TO MFS-IDPFK                                              
019500     END-IF                                                               
019600                                                                          
019700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019800     MOVE '001'             TO MSGI-KDCALL                                
019900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020100     MOVE '4411'            TO MSGI-IDTRANS                               
020200     IF EGEN-MID                                                          
020300       MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                            
020400       MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                           
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020700     MOVE MSGI-IDSPRAK         TO MCNV-IDSPRAK                            
020800     MOVE MSGI-IDUSER          TO REQU-IDUSER                             
020900                                                                          
021000     .                                                                    
021100     EJECT                                                                
021200 B-INIT-KEYS SECTION.                                                     
021300                                                                          
021400     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
021500                                                                          
021600     IF MID-IDDISTR-IN NOT = ALL '+'                                      
021700       MOVE '7'         TO MFS-IDPFK                                      
021800       MOVE SPACE       TO MFS-KDTRTYP                                    
021900     END-IF                                                               
022000     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
022100     MOVE MSGI-IDDISTR  TO REQU-IDDISTR-KEY                               
022200                                                                          
022300*    -- KONTROLL AV IDKUNDNR                                              
022400     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
022500                                                                          
022600     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
022700       MOVE '7'         TO MFS-IDPFK                                      
022800       MOVE SPACE       TO MFS-KDTRTYP                                    
022900     END-IF                                                               
023000     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
023100     MOVE MSGI-IDKUNDNR TO REQU-IDKUNDNR-KEY                              
023200                                                                          
023300     IF GODK-MID                                                          
023400       MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                         
023500       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
023600       MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                        
023700       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
023800     ELSE                                                                 
023900       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
024000                               MOD-IDKUNDNR-UT                            
024100     END-IF                                                               
024200                                                                          
024300     .                                                                    
024400     EJECT                                                                
024500 C-INIT-REQU SECTION.                                                     
024600                                                                          
024700     MOVE MID-IDKUNDNR-COPY      TO REQU-IDKUNDNR-COPY                    
024800     MOVE MID-BEGMT-RAD1         TO REQU-BEGMT-RAD1                       
024810     MOVE MID-IDPARTNER          TO REQU-IDPARTNER                        
024900     MOVE MID-BEGMT-RAD2         TO REQU-BEGMT-RAD2                       
024910     MOVE ALL '+'                TO REQU-IDDEALER-VIPS                    
025000     MOVE MID-ADGMT-GATA         TO REQU-ADGMT-GATA                       
025010     MOVE MID-IDLANDX2-IN        TO REQU-IDLANDX2                         
025100     MOVE MID-ADPOSTNR           TO REQU-ADPOSTNR                         
025200     MOVE MID-KDPOSTNR           TO REQU-KDPOSTNR                         
025210     MOVE MID-KDKUNDKAT-IN       TO REQU-KDKUNDKAT                        
025300     MOVE MID-ADCITY             TO REQU-ADCITY                           
025310     MOVE MID-IDLONGITUDE-IN     TO REQU-IDLONGITUDE                      
025400     MOVE MID-ADGMT-LAND         TO REQU-ADGMT-LAND                       
025410     MOVE MID-IDLATITUDE-IN      TO REQU-IDLATITUDE                       
025500     MOVE MID-IDTFN              TO REQU-IDTFN                            
025510     MOVE MID-BETEXT-IN          TO REQU-BETEXT                           
025600     MOVE MID-FLRESTN            TO REQU-FLRESTN                          
025700     MOVE MID-FLPRELRO           TO REQU-FLPRELRO                         
025800     MOVE MID-RESLATT-IN         TO REQU-RESLATT-UPD                      
025900     MOVE MID-IDKUNDNR-H-IN      TO REQU-IDKUNDNR-H-UPD                   
026000     MOVE MID-KDBEKALT-IN        TO REQU-KDBEKALT-UPD                     
026100     MOVE MID-FLOBKR-TACD-IN     TO REQU-FLOBKR-TACD-UPD                  
026200     MOVE MID-FLDNDAP-IN         TO REQU-FLDNDAP-UPD                      
026300     MOVE MID-FLORDTIL-KL1       TO REQU-FLORDTIL-KL1                     
026400     MOVE MID-FLORDTIL-KL2       TO REQU-FLORDTIL-KL2                     
026500     MOVE MID-FLORDTIL-KL3       TO REQU-FLORDTIL-KL3                     
026600     MOVE MID-FLORDTIL-KL4       TO REQU-FLORDTIL-KL4                     
029200     .                                                                    
029300     EJECT                                                                
029400 E-SAMMA-SIDA SECTION.                                                    
029500                                                                          
029600     IF EGEN-MID OR HELP-MID                                              
029700       CONTINUE                                                           
029800     ELSE                                                                 
029900       PERFORM MFS-RENSA-FAELT-IN                                         
030000     END-IF                                                               
030100     .                                                                    
030200     EJECT                                                                
030300 F-CALL-BIZ-LOGIC-W4041110 SECTION.                                       
030400                                                                          
030500     CALL W4041110 USING REQU-AREA RESP-AREA                              
030600                         WDB2-PCB                                         
030700     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
030800        RESP-IDMSG-INFO  NOT = SPACE                                      
030900       PERFORM FA-SET-MSG-AND-HILIGHT                                     
031000     END-IF                                                               
031100     PERFORM FB-MOVE-RESP-TO-MOD                                          
031200                                                                          
031300     .                                                                    
031400     EJECT                                                                
031500                                                                          
031600 FA-SET-MSG-AND-HILIGHT SECTION.                                          
031700                                                                          
031800     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
031900     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
032000     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
032100                                                                          
032200     CALL WL01MCNV            USING MCNV-AREA                             
032300     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
032400     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
032500                                                                          
032600     .                                                                    
032700     EJECT                                                                
032800                                                                          
032900 FB-MOVE-RESP-TO-MOD SECTION.                                             
033000                                                                          
033100     MOVE RESP-IDKUNDNR-COPY-ATTR                                         
033200                                 TO MOD-IDKUNDNR-COPY-ATTR                
033300     IF RESP-IDKUNDNR-COPY = SPACE                                        
033400       MOVE MFS-RENSA-FAELT      TO MOD-IDKUNDNR-COPY                     
033500     ELSE                                                                 
033600       IF RESP-IDKUNDNR-COPY = ALL '+'                                    
033700         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKUNDNR-COPY                     
033800       ELSE                                                               
033900         MOVE RESP-IDKUNDNR-COPY TO MOD-IDKUNDNR-COPY                     
034000       END-IF                                                             
034100     END-IF                                                               
034200                                                                          
034300     MOVE RESP-BEGMT-RAD1-ATTR                                            
034400                                 TO MOD-BEGMT-RAD1-ATTR                   
034500     IF RESP-BEGMT-RAD1 = SPACE                                           
034600       MOVE MFS-RENSA-FAELT      TO MOD-BEGMT-RAD1                        
034700     ELSE                                                                 
034800       IF RESP-BEGMT-RAD1 = ALL '+'                                       
034900         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEGMT-RAD1                        
035000       ELSE                                                               
035100         MOVE RESP-BEGMT-RAD1    TO MOD-BEGMT-RAD1                        
035200       END-IF                                                             
035300     END-IF                                                               
035400                                                                          
035410     MOVE RESP-IDPARTNER-ATTR                                             
035420                                 TO MOD-IDPARTNER-ATTR                    
035430     IF RESP-IDPARTNER = SPACE                                            
035440       MOVE MFS-RENSA-FAELT      TO MOD-IDPARTNER                         
035450     ELSE                                                                 
035460       IF RESP-IDPARTNER = ALL '+'                                        
035470         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDPARTNER                         
035480       ELSE                                                               
035490         MOVE RESP-IDPARTNER     TO MOD-IDPARTNER                         
035491       END-IF                                                             
035492     END-IF                                                               
035493                                                                          
035510     MOVE RESP-BEGMT-RAD2-ATTR                                            
035600                                 TO MOD-BEGMT-RAD2-ATTR                   
035700     IF RESP-BEGMT-RAD2 = SPACE                                           
035800       MOVE MFS-RENSA-FAELT      TO MOD-BEGMT-RAD2                        
035900     ELSE                                                                 
036000       IF RESP-BEGMT-RAD2 = ALL '+'                                       
036100         MOVE MFS-ROER-EJ-FAELT  TO MOD-BEGMT-RAD2                        
036200       ELSE                                                               
036300         MOVE RESP-BEGMT-RAD2    TO MOD-BEGMT-RAD2                        
036400       END-IF                                                             
036500     END-IF                                                               
036501                                                                          
036510     IF RESP-IDDEALER-VIPS = SPACE                                        
036520       MOVE MFS-RENSA-FAELT      TO MOD-IDDEALER-VIPS                     
036530     ELSE                                                                 
036540       IF RESP-IDDEALER-VIPS = ALL '+'                                    
036550         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDDEALER-VIPS                     
036560       ELSE                                                               
036570         MOVE RESP-IDDEALER-VIPS TO MOD-IDDEALER-VIPS                     
036580       END-IF                                                             
036590     END-IF                                                               
036591                                                                          
036700     MOVE RESP-ADGMT-GATA-ATTR                                            
036800                                 TO MOD-ADGMT-GATA-ATTR                   
036900     IF RESP-ADGMT-GATA = SPACE                                           
037000       MOVE MFS-RENSA-FAELT      TO MOD-ADGMT-GATA                        
037100     ELSE                                                                 
037200       IF RESP-ADGMT-GATA = ALL '+'                                       
037300         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADGMT-GATA                        
037400       ELSE                                                               
037500         MOVE RESP-ADGMT-GATA    TO MOD-ADGMT-GATA                        
037600       END-IF                                                             
037700     END-IF                                                               
037800                                                                          
037801     MOVE RESP-IDLANDX2-ATTR                                              
037802                                 TO MOD-IDLANDX2-ATTR                     
037810     IF RESP-IDLANDX2 = SPACE                                             
037820       MOVE MFS-RENSA-FAELT      TO MOD-IDLANDX2                          
037830     ELSE                                                                 
037840       IF RESP-IDLANDX2 = ALL '+'                                         
037850         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLANDX2                          
037860       ELSE                                                               
037870         MOVE RESP-IDLANDX2      TO MOD-IDLANDX2                          
037880       END-IF                                                             
037890     END-IF                                                               
037891                                                                          
037910     MOVE RESP-ADPOSTNR-ATTR                                              
038000                                 TO MOD-ADPOSTNR-ATTR                     
038100     IF RESP-ADPOSTNR = SPACE                                             
038200       MOVE MFS-RENSA-FAELT      TO MOD-ADPOSTNR                          
038300     ELSE                                                                 
038400       IF RESP-ADPOSTNR = ALL '+'                                         
038500         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADPOSTNR                          
038600       ELSE                                                               
038700         MOVE RESP-ADPOSTNR      TO MOD-ADPOSTNR                          
038800       END-IF                                                             
038900     END-IF                                                               
039000                                                                          
039100     MOVE RESP-KDPOSTNR-ATTR                                              
039200                                 TO MOD-KDPOSTNR-ATTR                     
039300     IF RESP-KDPOSTNR = SPACE                                             
039400       MOVE MFS-RENSA-FAELT      TO MOD-KDPOSTNR                          
039500     ELSE                                                                 
039600       IF RESP-KDPOSTNR = ALL '+'                                         
039700         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDPOSTNR                          
039800       ELSE                                                               
039900         MOVE RESP-KDPOSTNR      TO MOD-KDPOSTNR                          
040000       END-IF                                                             
040100     END-IF                                                               
040101                                                                          
040102     MOVE RESP-KDKUNDKAT-ATTR    TO MOD-KDKUNDKAT-ATTR                    
040110     IF RESP-KDKUNDKAT = SPACE                                            
040120       MOVE MFS-RENSA-FAELT      TO MOD-KDKUNDKAT                         
040121       MOVE SPACES               TO MOD-BEKUNDKAT                         
040130     ELSE                                                                 
040140       IF RESP-KDKUNDKAT = ALL '+'                                        
040150         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDKUNDKAT                         
040151         MOVE SPACES             TO MOD-BEKUNDKAT                         
040160       ELSE                                                               
040170         MOVE RESP-KDKUNDKAT     TO MOD-KDKUNDKAT                         
040171         MOVE SPACES             TO MOD-BEKUNDKAT                         
040172                                                                          
040173         MOVE MOD-KDKUNDKAT      TO WS-KDKUNDKAT-REC                      
040174                                                                          
040175         SET TEXT02-IX           TO 1                                     
040176         SEARCH ALL TEXT02-KDKUNDKAT-TAB                                  
040177           AT END                                                         
040178             MOVE MFS-RENSA-FAELT  TO MOD-BEKUNDKAT                       
040179                                                                          
040180           WHEN TEXT02-KDKUNDKAT-REC(TEXT02-IX)                           
040181             = WS-KDKUNDKAT                                               
040182               MOVE TEXT02-BEKUNDKAT(TEXT02-IX)                           
040183                                   TO MOD-BEKUNDKAT                       
040184         END-SEARCH                                                       
040185       END-IF                                                             
040190     END-IF                                                               
040191                                                                          
040300     MOVE RESP-ADCITY-ATTR                                                
040400                                 TO MOD-ADCITY-ATTR                       
040500     IF RESP-ADCITY = SPACE                                               
040600       MOVE MFS-RENSA-FAELT      TO MOD-ADCITY                            
040700     ELSE                                                                 
040800       IF RESP-ADCITY = ALL '+'                                           
040900         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADCITY                            
041000       ELSE                                                               
041100         MOVE RESP-ADCITY        TO MOD-ADCITY                            
041200       END-IF                                                             
041300     END-IF                                                               
041400                                                                          
041410     MOVE RESP-IDLONGITUDE-ATTR                                           
041420                                 TO MOD-IDLONGITUDE-ATTR                  
041430     IF RESP-IDLONGITUDE = SPACE                                          
041440       MOVE MFS-RENSA-FAELT      TO MOD-IDLONGITUDE                       
041450     ELSE                                                                 
041460       IF RESP-IDLONGITUDE = ALL '+'                                      
041470         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLONGITUDE                       
041480       ELSE                                                               
041490         MOVE RESP-IDLONGITUDE   TO MOD-IDLONGITUDE                       
041491       END-IF                                                             
041492     END-IF                                                               
041493                                                                          
041500     MOVE RESP-ADGMT-LAND-ATTR                                            
041600                                 TO MOD-ADGMT-LAND-ATTR                   
041700     IF RESP-ADGMT-LAND = SPACE                                           
041800       MOVE MFS-RENSA-FAELT      TO MOD-ADGMT-LAND                        
041900     ELSE                                                                 
042000       IF RESP-ADGMT-LAND = ALL '+'                                       
042100         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADGMT-LAND                        
042200       ELSE                                                               
042300         MOVE RESP-ADGMT-LAND    TO MOD-ADGMT-LAND                        
042400       END-IF                                                             
042500     END-IF                                                               
042600                                                                          
042610     MOVE RESP-IDLATITUDE-ATTR   TO MOD-IDLATITUDE-ATTR                   
042620                                                                          
042630     IF RESP-IDLATITUDE = SPACE                                           
042640       MOVE MFS-RENSA-FAELT      TO MOD-IDLATITUDE                        
042650     ELSE                                                                 
042660       IF RESP-IDLATITUDE = ALL '+'                                       
042670         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLATITUDE                        
042680       ELSE                                                               
042690         MOVE RESP-IDLATITUDE    TO MOD-IDLATITUDE                        
042691       END-IF                                                             
042692     END-IF                                                               
042693                                                                          
042700     MOVE RESP-IDTFN-ATTR                                                 
042800                                 TO MOD-IDTFN-ATTR                        
042900     IF RESP-IDTFN = SPACE                                                
043000       MOVE MFS-RENSA-FAELT      TO MOD-IDTFN                             
043100     ELSE                                                                 
043200       IF RESP-IDTFN = ALL '+'                                            
043300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTFN                             
043400       ELSE                                                               
043500         MOVE RESP-IDTFN         TO MOD-IDTFN                             
043600       END-IF                                                             
043700     END-IF                                                               
043800                                                                          
043810     IF RESP-BETEXT = SPACE                                               
043820       MOVE MFS-RENSA-FAELT      TO MOD-BETEXT                            
043830     ELSE                                                                 
043840       IF RESP-BETEXT = ALL '+'                                           
043850         MOVE MFS-ROER-EJ-FAELT  TO MOD-BETEXT                            
043860       ELSE                                                               
043870         MOVE RESP-BETEXT        TO MOD-BETEXT                            
043880       END-IF                                                             
043890     END-IF                                                               
043891                                                                          
043910     MOVE RESP-FLRESTN-ATTR                                               
044000                                 TO MOD-FLRESTN-ATTR                      
044100     IF RESP-FLRESTN = SPACE                                              
044200       MOVE MFS-RENSA-FAELT      TO MOD-FLRESTN                           
044300     ELSE                                                                 
044400       IF RESP-FLRESTN = ALL '+'                                          
044500         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLRESTN                           
044600       ELSE                                                               
044700         MOVE RESP-FLRESTN       TO MOD-FLRESTN                           
044800       END-IF                                                             
044900     END-IF                                                               
045000                                                                          
045100     IF RESP-IDZON = SPACE                                                
045200       MOVE MFS-RENSA-FAELT      TO MOD-IDZON                             
045300     ELSE                                                                 
045400       IF RESP-IDZON = ALL '+'                                            
045500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDZON                             
045600       ELSE                                                               
045700         MOVE RESP-IDZON         TO MOD-IDZON                             
045800       END-IF                                                             
045900     END-IF                                                               
046000                                                                          
046100     MOVE RESP-FLPRELRO-ATTR                                              
046200                                 TO MOD-FLPRELRO-ATTR                     
046300     IF RESP-FLPRELRO = SPACE                                             
046400       MOVE MFS-RENSA-FAELT      TO MOD-FLPRELRO                          
046500     ELSE                                                                 
046600       IF RESP-FLPRELRO = ALL '+'                                         
046700         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLPRELRO                          
046800       ELSE                                                               
046900         MOVE RESP-FLPRELRO      TO MOD-FLPRELRO                          
047000       END-IF                                                             
047100     END-IF                                                               
047200                                                                          
047300     IF RESP-IDDEPOT = SPACE                                              
047400       MOVE MFS-RENSA-FAELT      TO MOD-IDDEPOT                           
047500     ELSE                                                                 
047600       IF RESP-IDDEPOT = ALL '+'                                          
047700         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDDEPOT                           
047800       ELSE                                                               
047900         MOVE RESP-IDDEPOT       TO MOD-IDDEPOT                           
048000       END-IF                                                             
048100     END-IF                                                               
048200                                                                          
048300     IF RESP-RESLATT-UT = SPACE                                           
048400       MOVE MFS-RENSA-FAELT      TO MOD-RESLATT-UT                        
048500     ELSE                                                                 
048600       IF RESP-RESLATT-UT = ALL '+'                                       
048700         MOVE MFS-ROER-EJ-FAELT  TO MOD-RESLATT-UT                        
048800       ELSE                                                               
048900         MOVE RESP-RESLATT-UT    TO MOD-RESLATT-UT                        
049000       END-IF                                                             
049100     END-IF                                                               
049200                                                                          
049300     MOVE RESP-RESLATT-UPD-ATTR                                           
049400                                 TO MOD-RESLATT-IN-ATTR                   
049500     IF RESP-RESLATT-UPD = SPACE                                          
049600       MOVE MFS-RENSA-FAELT      TO MOD-RESLATT-IN                        
049700     ELSE                                                                 
049800       IF RESP-RESLATT-UPD = ALL '+'                                      
049900         MOVE MFS-ROER-EJ-FAELT  TO MOD-RESLATT-IN                        
050000       ELSE                                                               
050100         MOVE RESP-RESLATT-UPD   TO MOD-RESLATT-IN                        
050200       END-IF                                                             
050300     END-IF                                                               
050400                                                                          
050500     IF RESP-IDROUTE = SPACE                                              
050600       MOVE MFS-RENSA-FAELT      TO MOD-IDROUTE                           
050700     ELSE                                                                 
050800       IF RESP-IDROUTE = ALL '+'                                          
050900         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDROUTE                           
051000       ELSE                                                               
051100         MOVE RESP-IDROUTE       TO MOD-IDROUTE                           
051200       END-IF                                                             
051300     END-IF                                                               
051400                                                                          
051500     IF RESP-IDKUNDNR-H-UT = SPACE                                        
051600       MOVE MFS-RENSA-FAELT      TO MOD-IDKUNDNR-H-UT                     
051700     ELSE                                                                 
051800       IF RESP-IDKUNDNR-H-UT = ALL '+'                                    
051900         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKUNDNR-H-UT                     
052000       ELSE                                                               
052100         MOVE RESP-IDKUNDNR-H-UT TO MOD-IDKUNDNR-H-UT                     
052200       END-IF                                                             
052300     END-IF                                                               
052400                                                                          
052500     MOVE RESP-IDKUNDNR-H-UPD-ATTR                                        
052600                                 TO MOD-IDKUNDNR-H-IN-ATTR                
052700     IF RESP-IDKUNDNR-H-UPD = SPACE                                       
052800       MOVE MFS-RENSA-FAELT      TO MOD-IDKUNDNR-H-IN                     
052900     ELSE                                                                 
053000       IF RESP-IDKUNDNR-H-UPD = ALL '+'                                   
053100         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKUNDNR-H-IN                     
053200       ELSE                                                               
053300         MOVE RESP-IDKUNDNR-H-UPD                                         
053400                                 TO MOD-IDKUNDNR-H-IN                     
053500       END-IF                                                             
053600     END-IF                                                               
053700                                                                          
053800     IF RESP-KDBEKALT-UT = SPACE                                          
053900       MOVE MFS-RENSA-FAELT      TO MOD-KDBEKALT-UT                       
054000     ELSE                                                                 
054100       IF RESP-KDBEKALT-UT = ALL '+'                                      
054200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDBEKALT-UT                       
054300       ELSE                                                               
054400         MOVE RESP-KDBEKALT-UT   TO MOD-KDBEKALT-UT                       
054500       END-IF                                                             
054600     END-IF                                                               
054700                                                                          
054800     MOVE RESP-KDBEKALT-UPD-ATTR                                          
054900                                 TO MOD-KDBEKALT-IN-ATTR                  
055000     IF RESP-KDBEKALT-UPD = SPACE                                         
055100       MOVE MFS-RENSA-FAELT      TO MOD-KDBEKALT-IN                       
055200     ELSE                                                                 
055300       IF RESP-KDBEKALT-UPD = ALL '+'                                     
055400         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDBEKALT-IN                       
055500       ELSE                                                               
055600         MOVE RESP-KDBEKALT-UPD  TO MOD-KDBEKALT-IN                       
055700       END-IF                                                             
055800     END-IF                                                               
055900                                                                          
056000     IF RESP-FLOBKR-TACD-UT = SPACE                                       
056100       MOVE MFS-RENSA-FAELT      TO MOD-FLOBKR-TACD-UT                    
056200     ELSE                                                                 
056300       IF RESP-FLOBKR-TACD-UT = ALL '+'                                   
056400         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLOBKR-TACD-UT                    
056500       ELSE                                                               
056600         MOVE RESP-FLOBKR-TACD-UT                                         
056700                                 TO MOD-FLOBKR-TACD-UT                    
056800       END-IF                                                             
056900     END-IF                                                               
057000                                                                          
057100     MOVE RESP-FLOBKR-TACD-UPD-ATTR                                       
057200                                 TO MOD-FLOBKR-TACD-IN-ATTR               
057300     IF RESP-FLOBKR-TACD-UPD = SPACE                                      
057400       MOVE MFS-RENSA-FAELT      TO MOD-FLOBKR-TACD-IN                    
057500     ELSE                                                                 
057600       IF RESP-FLOBKR-TACD-UPD = ALL '+'                                  
057700         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLOBKR-TACD-IN                    
057800       ELSE                                                               
057900         MOVE RESP-FLOBKR-TACD-UPD                                        
058000                                 TO MOD-FLOBKR-TACD-IN                    
058100       END-IF                                                             
058200     END-IF                                                               
058300                                                                          
058400     IF RESP-FLDNDAP-UT = SPACE                                           
058500       MOVE MFS-RENSA-FAELT      TO MOD-FLDNDAP-UT                        
058600     ELSE                                                                 
058700       IF RESP-FLDNDAP-UT = ALL '+'                                       
058800         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLDNDAP-UT                        
058900       ELSE                                                               
059000         MOVE RESP-FLDNDAP-UT    TO MOD-FLDNDAP-UT                        
059100       END-IF                                                             
059200     END-IF                                                               
059300                                                                          
059400     MOVE RESP-FLDNDAP-UPD-ATTR                                           
059500                                 TO MOD-FLDNDAP-IN-ATTR                   
059600     IF RESP-FLDNDAP-UPD = SPACE                                          
059700       MOVE MFS-RENSA-FAELT      TO MOD-FLDNDAP-IN                        
059800     ELSE                                                                 
059900       IF RESP-FLDNDAP-UPD = ALL '+'                                      
060000         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLDNDAP-IN                        
060100       ELSE                                                               
060200         MOVE RESP-FLDNDAP-UPD   TO MOD-FLDNDAP-IN                        
060300       END-IF                                                             
060400     END-IF                                                               
060500                                                                          
060600     MOVE RESP-FLORDTIL-KL1-ATTR                                          
060700                                 TO MOD-FLORDTIL-KL1-ATTR                 
060800     IF RESP-FLORDTIL-KL1 = SPACE                                         
060900       MOVE MFS-RENSA-FAELT      TO MOD-FLORDTIL-KL1                      
061000     ELSE                                                                 
061100       IF RESP-FLORDTIL-KL1 = ALL '+'                                     
061200         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLORDTIL-KL1                      
061300       ELSE                                                               
061400         MOVE RESP-FLORDTIL-KL1  TO MOD-FLORDTIL-KL1                      
061500       END-IF                                                             
061600     END-IF                                                               
061700                                                                          
061800     MOVE RESP-FLORDTIL-KL2-ATTR                                          
061900                                 TO MOD-FLORDTIL-KL2-ATTR                 
062000     IF RESP-FLORDTIL-KL2 = SPACE                                         
062100       MOVE MFS-RENSA-FAELT      TO MOD-FLORDTIL-KL2                      
062200     ELSE                                                                 
062300       IF RESP-FLORDTIL-KL2 = ALL '+'                                     
062400         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLORDTIL-KL2                      
062500       ELSE                                                               
062600         MOVE RESP-FLORDTIL-KL2  TO MOD-FLORDTIL-KL2                      
062700       END-IF                                                             
062800     END-IF                                                               
062900                                                                          
063000     MOVE RESP-FLORDTIL-KL3-ATTR                                          
063100                                 TO MOD-FLORDTIL-KL3-ATTR                 
063200     IF RESP-FLORDTIL-KL3 = SPACE                                         
063300       MOVE MFS-RENSA-FAELT      TO MOD-FLORDTIL-KL3                      
063400     ELSE                                                                 
063500       IF RESP-FLORDTIL-KL3 = ALL '+'                                     
063600         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLORDTIL-KL3                      
063700       ELSE                                                               
063800         MOVE RESP-FLORDTIL-KL3  TO MOD-FLORDTIL-KL3                      
063900       END-IF                                                             
064000     END-IF                                                               
064100                                                                          
064200     MOVE RESP-FLORDTIL-KL4-ATTR                                          
064300                                 TO MOD-FLORDTIL-KL4-ATTR                 
064400     IF RESP-FLORDTIL-KL4 = SPACE                                         
064500       MOVE MFS-RENSA-FAELT      TO MOD-FLORDTIL-KL4                      
064600     ELSE                                                                 
064700       IF RESP-FLORDTIL-KL4 = ALL '+'                                     
064800         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLORDTIL-KL4                      
064900       ELSE                                                               
065000         MOVE RESP-FLORDTIL-KL4  TO MOD-FLORDTIL-KL4                      
065100       END-IF                                                             
065200     END-IF                                                               
065300                                                                          
075000     .                                                                    
075100     EJECT                                                                
075294                                                                          
075300 MFS-RENSA-FAELT-IN SECTION.                                              
075400                                                                          
075500*    --- ALLA INDATA-FÄLT                                                 
075600     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-COPY                            
075700                             MOD-BEGMT-RAD1                               
075710                             MOD-IDPARTNER                                
075800                             MOD-BEGMT-RAD2                               
075810                             MOD-IDDEALER-VIPS                            
075900                             MOD-ADGMT-GATA                               
075910                             MOD-IDLANDX2                                 
076000                             MOD-ADPOSTNR                                 
076100                             MOD-KDPOSTNR                                 
076110                             MOD-KDKUNDKAT                                
076200                             MOD-ADCITY                                   
076300                             MOD-ADGMT-LAND                               
076400                             MOD-IDTFN                                    
076500                             MOD-BETEXT                                   
076510                             MOD-FLRESTN                                  
076600                             MOD-FLPRELRO                                 
076700                             MOD-RESLATT-IN                               
076800                             MOD-IDKUNDNR-H-IN                            
076900                             MOD-KDBEKALT-IN                              
077000                             MOD-FLOBKR-TACD-IN                           
077100                             MOD-FLDNDAP-IN                               
077200                             MOD-FLORDTIL-KL1                             
077300                             MOD-FLORDTIL-KL2                             
077400                             MOD-FLORDTIL-KL3                             
077500                             MOD-FLORDTIL-KL4                             
078400     .                                                                    
078500     EJECT                                                                
078600* --- IMS SEKTIONER ---                                                   
078700     SKIP3                                                                
078800 IMS-GET-MSG SECTION.                                                     
078900                                                                          
079000     MOVE '  QC' TO GODK-STATUSKODER                                      
079100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
079200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
079300     PERFORM IMS-STATUSKONTROLL                                           
079400     .                                                                    
079500     SKIP3                                                                
079600 IMS-INSERT-MSG SECTION.                                                  
079700                                                                          
079800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
079900       MOVE 'N' TO MFS-KDHUVOMR                                           
080000     END-IF                                                               
080100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
080200     MOVE SPACE TO GODK-STATUSKODER                                       
080300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
080400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
080500     PERFORM IMS-STATUSKONTROLL                                           
080600     .                                                                    
080700     EJECT                                                                
080800 IMS-STATUSKONTROLL SECTION.                                              
080900                                                                          
081000     SET STATUS-IX TO 1                                                   
081100     SEARCH GODK-STATUS                                                   
081200       AT END                                                             
081300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
081400         DELIMITED BY SIZE INTO FELTEXT                                   
081500         CALL FELLOG                                                      
081600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
081700         CONTINUE                                                         
081800     END-SEARCH                                                           
081900     .                                                                    
082000     EJECT                                                                
