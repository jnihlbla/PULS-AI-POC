000400 ID DIVISION.                                                             
000600 PROGRAM-ID.     W1012100.                                                
001000*AUTHOR.         BODIL LINDAHL.                                           
001100*DATE-WRITTEN.   MAJ 1985.                                                
001500*    FUNKTION.                                                            
001600*        SÖKNING BENÄMNINGSTEXT.                                          
002000*                                                                         
002010*    ÄNDRING:                                                             
002020*        2000-11-20  TILLKOMMER TURKISKA  /C.E.                           
002021*    ÄNDRING:                                                             
002023*        2005-10-18  TECHLA-FLAGGAN "ASTERISK" BLANKAS FELAKTIGT          
002024*                    NÄR NÅGOT AV SPRÅKEN EJ ÄR ÖVERSATT.                 
002030*                    eTracker 2632709  /C.E.                              
002100*    INDATA.                                                              
002200*        TRANSAKTION: W1T121                                              
002300*        MID:         W1I12101                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W1O12101                                            
002700*                                                                         
002800*    SUBPROGRAM.                                                          
002900*        CBLTDLI                                                          
003000*        FELLOG                                                           
003100*        WREVERSE                                                         
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP3                                                                
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W1012100'.            
004500 77    JA                        PIC X       VALUE 'J'.                   
004600 77    NEJ                       PIC X       VALUE 'N'.                   
004700 77    SVENSKA                   PIC X(3)    VALUE 'S  '.                 
004800 77    ENGELSKA                  PIC X(3)    VALUE 'GB '.                 
004900 77    PORTUGISISKA              PIC X(3)    VALUE 'P  '.                 
005000 77    BRASILIANSKA              PIC X(3)    VALUE 'BR '.                 
005100 77    ASTERISK                  PIC X       VALUE '*'.                   
005200 77    BAK                       PIC X       VALUE 'B'.                   
005300 77    FRAM                      PIC X       VALUE 'F'.                   
005400 77    WS-BEART                  PIC X(25)   VALUE SPACE.                 
005500 77    WS-BEART-UT               PIC X(25)   VALUE SPACE.                 
005600 77    WS-IDSKYLT                PIC X(3)    VALUE SPACE.                 
005700 77    WS-IDSKYLT-SVAR           PIC X(3)    VALUE SPACE.                 
005800 77    WS-IDSKYLT-SOEK           PIC X(3)    VALUE SPACE.                 
005900 77    WS-KDSOEK                 PIC X(1)    VALUE SPACE.                 
006000 77    WS-BEART-SPAR             PIC X(25)   VALUE SPACE.                 
006100 77    WS-IDBENNR-SPAR           PIC 9(7)    VALUE ZERO.                  
006200 77    IX                        PIC S9(9)   VALUE +0   COMP SYNC.        
006300 77    RAD-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006400 77    MAX-RADER                 PIC S9(3)   VALUE +14.                   
006500 77    MAX-RADER-PLUS-1          PIC S9(3)   VALUE +15.                   
006700     SKIP3                                                                
006710                                                                          
006720 01  DYNAMISKA-SUBPROGRAM.                                                
006730   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006740   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006750                                                                          
006800 01    SPRAAK-KOLL          PIC X.                                        
006900   88  SPRAAK-FINNS         VALUE 'J'.                                    
007000     SKIP2                                                                
007100 01    INPUT-RETT           PIC X            VALUE 'J'.                   
007200     SKIP2                                                                
007300 01    SUBPROGRAM.                                                        
007400   03  WREVERSE             PIC X(8)        VALUE 'WREVERSE'.             
007500     EJECT                                                                
007600*01    -COPY WREVAREA                                                     
007800     EJECT                                                                
007900*01    -COPY WWLAND03                                                     
008100     EJECT                                                                
008200 01    NYCKLAR-TILL-DLI.                                                  
008300   03    W-BEART-X.                                                       
008400     05    W-BEART               PIC X(25)   VALUE SPACE.                 
008500   03    W-BEART-MAX-X.                                                   
008600     05    W-BEART-MAX           PIC X(25)   VALUE SPACE.                 
008700   03    W-IDSKYLT-SOEK-X.                                                
008800     05    W-IDSKYLT-SOEK        PIC X(3)    VALUE SPACE.                 
008900   03    W-IDSKYLT-SVAR-X.                                                
009000     05    W-IDSKYLT-SVAR        PIC X(3)    VALUE SPACE.                 
009100   03    W-IDSKYLT-X.                                                     
009200     05    W-IDSKYLT             PIC X(3)    VALUE SPACE.                 
009300     SKIP3                                                                
009400 01    MEDDELANDE.                                                        
009500   03    W-MED-1.                                                         
009600         05  FILLER              PIC X(33)  VALUE                         
009700             'FLER RADER FINNS                 '.                         
009800         05  FILLER              PIC X(33)  VALUE                         
009900             'FOR MORE INFORMATION, PRESS ENTER'.                         
010000   03    FILLER REDEFINES W-MED-1.                                        
010100         05 MED-1                PIC X(33) OCCURS 2.                      
010200                                                                          
010300   03    W-FEL-1.                                                         
010400         05  FILLER              PIC X(33)  VALUE                         
010500             'UPPLYSTA FÄLT FEL                '.                         
010600         05  FILLER              PIC X(33)  VALUE                         
010700             'HIGH LIGHTED FIELD INCORRECT     '.                         
010800   03    FILLER REDEFINES W-FEL-1.                                        
010900         05 FEL-1                PIC X(33) OCCURS 2.                      
011000                                                                          
011100   03    W-FEL-2.                                                         
011200         05  FILLER              PIC X(33)  VALUE                         
011300             'TEXT SAKNAS                      '.                         
011400         05  FILLER              PIC X(33)  VALUE                         
011500             'DESCRIPTION IS MISSING           '.                         
011600   03    FILLER REDEFINES W-FEL-2.                                        
011700         05 FEL-2                PIC X(33) OCCURS 2.                      
011800     EJECT                                                                
011900******************************************************************        
012000*                                                                         
012100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
012200*                                                                         
012300 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
012400     SKIP3                                                                
012500*01    MID -COPY W1I12101.                                                
012700     EJECT                                                                
012800*01    -COPY WMSGAREA                                                     
013000     EJECT                                                                
013100*  03    MOD -COPY W1O12101  -RED MSG-AREA.                               
013300     EJECT                                                                
013400*01    -COPY WMFSAREA                                                     
013600     EJECT                                                                
013700******************************************************************        
013800*                                                                         
013900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000*                                                                         
014100 01    IMS-WS.                                                            
014200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
014300     SKIP3                                                                
014400*                        **** STATUS-KOD FRÅN IMS                         
014500   03    STATUS-WS               PIC XX.                                  
014600     88    SEGMENT-FINNS                     VALUE '  '.                  
014700     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
014800     88    BASEN-SLUT                        VALUE 'GB'.                  
014900     SKIP3                                                                
015000   03    GODK-STATUSKODER.                                                
015100     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
015200     SKIP3                                                                
015300 01    SSA1                      PIC X(128).                              
015400     EJECT                                                                
015500*                            IMS FUNKTIONSKODER                           
015600*01    -COPY W0003                                                        
015800     EJECT                                                                
015900*                            DLI INPUT-OUTPUT AREA                        
016000 01    DLI-IO-AREA-1.                                                     
016100   03    IO-AREA-1               PIC X(200)  VALUE SPACE.                 
016200     SKIP3                                                                
016300*  03    WLBENA01 -COPY WDD301  -PRE BENA-  -RED IO-AREA-1.               
016500     EJECT                                                                
016600 01    DLI-IO-AREA-2.                                                     
016700   03    IO-AREA-2               PIC X(200)  VALUE SPACE.                 
016800     SKIP3                                                                
016900*  03    WLBENA11 -COPY WDD311  -PRE BENA-  -RED IO-AREA-2.               
017100     EJECT                                                                
017200 LINKAGE SECTION.                                                         
017300*01    -COPY W0009     -PRE MSG-                                          
017500     EJECT                                                                
017600*01    -COPY W0008     -PRE BENA-                                         
017800     05  FILLER                  PIC X.                                   
017900     EJECT                                                                
018000*01    -COPY W0008     -PRE BENB-                                         
018200     05  FILLER                  PIC X.                                   
018300     EJECT                                                                
018400 PROCEDURE DIVISION USING MSG-PCB BENA-PCB BENB-PCB.                      
018500     ENTRY 'DLITCBL' USING MSG-PCB BENA-PCB BENB-PCB                      
018600     SKIP2                                                                
018700     PERFORM IMS-GET-MSG                                                  
018800     IF SEGMENT-FINNS                                                     
018900       PERFORM A-INIT-SPARA-INPUT                                         
018910                                                                          
019000       IF INPUT-RETT = JA                                                 
019100         PERFORM B-LAS-BASEN                                              
019200       ELSE                                                               
019300         MOVE FEL-1(IX) TO MOD-TEMFSFEL                                   
019400       END-IF                                                             
019410                                                                          
019500       MOVE WS-IDSKYLT-SOEK TO MOD-IDSKYLT-SOEK-UT                        
019600       MOVE WS-IDSKYLT-SVAR TO MOD-IDSKYLT-SVAR-UT                        
019700       MOVE WS-KDSOEK       TO MOD-KDSOEK-UT                              
019800       MOVE WS-BEART-SPAR   TO MOD-BEART-SPAR                             
019900       MOVE WS-IDBENNR-SPAR TO MOD-IDBENNR-SPAR                           
020000       MOVE WS-BEART-UT     TO MOD-BEART-UT                               
020100       COMPUTE MSG-KVLL = 4 + ( LENGTH OF MOD-W1O12101-CTX )              
020200       PERFORM IMS-INSERT-MSG                                             
020300     END-IF                                                               
020400     MOVE ZERO TO RETURN-CODE                                             
020500     GOBACK                                                               
020600     .                                                                    
020700     EJECT                                                                
020800 A-INIT-SPARA-INPUT SECTION.                                              
020900     SKIP2                                                                
021000     MOVE JA TO INPUT-RETT                                                
021100                                                                          
021200     IF MSG-DUBBLA-TRANSKODER                                             
021300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I12101-CTX             
021400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
021500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021600     ELSE                                                                 
021700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I12101-CTX              
021800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
021900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022000     END-IF                                                               
022010                                                                          
022100     IF SWEDISH-TEXT                                                      
022200       MOVE +1 TO IX                                                      
022300     ELSE                                                                 
022400       MOVE +2 TO IX                                                      
022500     END-IF                                                               
022510                                                                          
022600     MOVE LOW-VALUE TO MSG-AREA                                           
022700     MOVE 'W1O121N1' TO MFS-IDMOD                                         
022800     MOVE '1121' TO MOD-IDTRANS                                           
022900     MOVE MFS-RENSA-FAELT TO MOD-BEART-IN                                 
023000                             MOD-IDSKYLT-SOEK-IN                          
023100                             MOD-IDSKYLT-SVAR-IN                          
023200                             MOD-KDSOEK-IN                                
023300                             MOD-TEMFSINF                                 
023400                             MOD-TEMFSFEL                                 
023500                                                                          
023600     IF MFS-IDTRANS = '1121'                                              
023700       MOVE MID-BEART-SPAR TO WS-BEART-SPAR                               
023800       IF MID-IDBENNR-SPAR NUMERIC                                        
023900         MOVE MID-IDBENNR-SPAR TO WS-IDBENNR-SPAR                         
024000       ELSE                                                               
024100         MOVE ZERO TO WS-IDBENNR-SPAR                                     
024200       END-IF                                                             
024300     ELSE                                                                 
024400       MOVE SPACE TO WS-BEART-SPAR                                        
024500       MOVE ZERO TO WS-IDBENNR-SPAR                                       
024600       MOVE 'S  ' TO MID-IDSKYLT-SOEK-IN                                  
024700       MOVE 'GB ' TO MID-IDSKYLT-SVAR-IN                                  
024800       MOVE 'F'   TO MID-KDSOEK-IN                                        
024900       IF MFS-IDTRANS = '1122'                                            
025000         CONTINUE                                                         
025100       ELSE                                                               
025200         MOVE SPACE TO MID-BEART-IN                                       
025300       END-IF                                                             
025400     END-IF                                                               
025410                                                                          
025500     IF MID-BEART-IN = ALL '+'                                            
025600       MOVE MID-BEART-UT TO WS-BEART                                      
025700                            WS-BEART-UT                                   
025800     ELSE                                                                 
025900       MOVE MID-BEART-IN TO WS-BEART                                      
026000                            WS-BEART-UT                                   
026100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEART-ATTR                        
026200       MOVE SPACE TO WS-BEART-SPAR                                        
026300       MOVE ZERO TO WS-IDBENNR-SPAR                                       
026400     END-IF                                                               
026410                                                                          
026500     IF MID-IDSKYLT-SOEK-IN = ALL '+' OR SPACE                            
026600       IF MID-IDSKYLT-SOEK-UT = SPACE                                     
026700         MOVE SVENSKA TO WS-IDSKYLT-SOEK                                  
026800                         WS-IDSKYLT                                       
026900         MOVE SPACE TO WS-BEART-SPAR                                      
027000         MOVE ZERO TO WS-IDBENNR-SPAR                                     
027100       ELSE                                                               
027200         MOVE MID-IDSKYLT-SOEK-UT TO WS-IDSKYLT-SOEK                      
027300                                    WS-IDSKYLT                            
027400       END-IF                                                             
027500     ELSE                                                                 
027600       MOVE MID-IDSKYLT-SOEK-IN TO WS-IDSKYLT-SOEK                        
027700                                   WS-IDSKYLT                             
027800       MOVE SPACE TO WS-BEART-SPAR                                        
027900       MOVE ZERO TO WS-IDBENNR-SPAR                                       
028000     END-IF                                                               
028010                                                                          
028100     IF WS-IDSKYLT-SOEK = SVENSKA OR ENGELSKA                             
028200       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-SOEK-ATTR                 
028300     ELSE                                                                 
028400       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-SOEK-ATTR                   
028500       MOVE NEJ TO INPUT-RETT                                             
028600     END-IF                                                               
028610                                                                          
028700     IF MID-IDSKYLT-SVAR-IN = ALL '+' OR SPACE                            
028800       IF MID-IDSKYLT-SVAR-UT = SPACE OR ALL '+'                          
028900         MOVE ENGELSKA TO WS-IDSKYLT-SVAR                                 
029000         MOVE SPACE TO WS-BEART-SPAR                                      
029100         MOVE ZERO TO WS-IDBENNR-SPAR                                     
029200       ELSE                                                               
029300         MOVE MID-IDSKYLT-SVAR-UT TO WS-IDSKYLT-SVAR                      
029400       END-IF                                                             
029500     ELSE                                                                 
029600       MOVE MID-IDSKYLT-SVAR-IN TO WS-IDSKYLT-SVAR                        
029700       MOVE SPACE TO WS-BEART-SPAR                                        
029800       MOVE ZERO TO WS-IDBENNR-SPAR                                       
029900     END-IF                                                               
029910                                                                          
030000     SET WWLAND03-IX TO +1                                                
030100     SEARCH WWLAND03-IDSKYLT-RAD AT END MOVE NEJ TO SPRAAK-KOLL           
030300       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT-SVAR               
030500         MOVE JA TO SPRAAK-KOLL                                           
030600     END-SEARCH                                                           
030610                                                                          
030700     IF SPRAAK-FINNS                                                      
030800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-SVAR-ATTR                 
030900     ELSE                                                                 
031000       IF WS-IDSKYLT-SVAR = BRASILIANSKA                                  
031100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-SVAR-ATTR               
031200       ELSE                                                               
031300         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-SVAR-ATTR                 
031400         MOVE NEJ TO INPUT-RETT                                           
031500       END-IF                                                             
031600     END-IF                                                               
031610                                                                          
031700     IF MID-KDSOEK-IN = ALL '+' OR SPACE                                  
031800       IF MID-KDSOEK-UT = SPACE  OR ALL '+'                               
031900         MOVE FRAM TO WS-KDSOEK                                           
032000         MOVE SPACE TO WS-BEART-SPAR                                      
032100         MOVE ZERO TO WS-IDBENNR-SPAR                                     
032200       ELSE                                                               
032300         MOVE MID-KDSOEK-UT TO WS-KDSOEK                                  
032400       END-IF                                                             
032500     ELSE                                                                 
032600       MOVE MID-KDSOEK-IN TO WS-KDSOEK                                    
032700       MOVE SPACE TO WS-BEART-SPAR                                        
032800       MOVE ZERO TO WS-IDBENNR-SPAR                                       
032900     END-IF                                                               
032910                                                                          
033000     IF WS-KDSOEK = BAK OR FRAM                                           
033100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSOEK-ATTR                       
033200       IF WS-KDSOEK = BAK                                                 
033300         IF WS-IDSKYLT-SOEK = SVENSKA                                     
033400           MOVE '  S' TO WS-IDSKYLT                                       
033500         ELSE                                                             
033600           MOVE ' BG' TO WS-IDSKYLT                                       
033700         END-IF                                                           
033800       END-IF                                                             
033900     ELSE                                                                 
034000       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSOEK-ATTR                         
034100       MOVE NEJ TO INPUT-RETT                                             
034200     END-IF                                                               
034300     .                                                                    
034400     EJECT                                                                
034500 B-LAS-BASEN SECTION.                                                     
034600     SKIP2                                                                
034700     MOVE 1 TO RAD-IX                                                     
034800                                                                          
034900     IF WS-BEART-SPAR = SPACE                                             
035000       IF WS-KDSOEK = BAK                                                 
035100         MOVE SPACE TO REV-TETEXT                                         
035200         MOVE WS-BEART TO REV-TETEXT                                      
035300         CALL WREVERSE USING REV-TETEXT                                   
035400         MOVE REV-TETEXT TO WS-BEART                                      
035500       END-IF                                                             
035600       MOVE WS-BEART TO W-BEART                                           
035700     ELSE                                                                 
035800       IF WS-KDSOEK = BAK                                                 
035900         MOVE SPACE TO REV-TETEXT                                         
036000         MOVE WS-BEART-SPAR TO REV-TETEXT                                 
036100         CALL WREVERSE USING REV-TETEXT                                   
036200         MOVE REV-TETEXT TO WS-BEART-SPAR                                 
036300                                                                          
036400         MOVE SPACE TO REV-TETEXT                                         
036500         MOVE WS-BEART TO REV-TETEXT                                      
036600         CALL WREVERSE USING REV-TETEXT                                   
036700         MOVE REV-TETEXT TO WS-BEART                                      
036900       END-IF                                                             
037000       MOVE WS-BEART-SPAR TO W-BEART                                      
037100     END-IF                                                               
037110                                                                          
037200     MOVE WS-BEART TO W-BEART-MAX                                         
037300     INSPECT W-BEART-MAX REPLACING ALL SPACE BY HIGH-VALUE                
037400                                                                          
037500     MOVE WS-IDSKYLT TO W-IDSKYLT                                         
037600     MOVE WS-IDSKYLT-SOEK TO W-IDSKYLT-SOEK                               
037610                                                                          
037700     IF WS-IDSKYLT-SVAR = BRASILIANSKA                                    
037800       MOVE PORTUGISISKA TO W-IDSKYLT-SVAR                                
037900     ELSE                                                                 
038000       MOVE WS-IDSKYLT-SVAR TO W-IDSKYLT-SVAR                             
038100     END-IF                                                               
038110                                                                          
038200     PERFORM UNTIL  RAD-IX >= MAX-RADER-PLUS-1                            
038400       PERFORM IMS-GET-BENA01-ASEQ                                        
038500       IF RAD-IX = +1                                                     
038600         IF WS-IDBENNR-SPAR = ZERO                                        
038700           CONTINUE                                                       
038800         ELSE                                                             
038900           PERFORM UNTIL SEGMENT-FINNS                                    
039000                   OR BENA-BEN-IDBENNR = WS-IDBENNR-SPAR                  
039200             PERFORM IMS-GET-BENA01-ASEQ                                  
039300           END-PERFORM                                                    
039400         END-IF                                                           
039500       END-IF                                                             
039600       IF SEGMENT-FINNS                                                   
039700         IF BENA-BEN-KDBENSTAT = 0                                        
039710*          --- FLAGGA FÖR ATT DET ÄR EN GODK TECHLA-BENÄMNING             
039800           MOVE ASTERISK TO MOD-KDBENSTAT(RAD-IX)                         
039900         ELSE                                                             
040000           MOVE SPACE    TO MOD-KDBENSTAT(RAD-IX)                         
041100         END-IF                                                           
041110                                                                          
041200         PERFORM IMS-GET-BENA11-SOEK                                      
041300         MOVE BENA-TEXT-BEART TO MOD-BEART-SOEK(RAD-IX)                   
041400         MOVE BENA-BEN-KDHOMONYM TO MOD-KDHOMONYM(RAD-IX)                 
041500         MOVE BENA-BEN-IDBENNR TO MOD-IDBENNR(RAD-IX)                     
041600         PERFORM IMS-GET-BENA11-SVAR                                      
041700         IF SEGMENT-FINNS                                                 
041800           MOVE BENA-TEXT-BEART TO MOD-BEART-SVAR(RAD-IX)                 
041900         END-IF                                                           
042000         ADD +1 TO RAD-IX                                                 
042100         IF RAD-IX = MAX-RADER-PLUS-1                                     
042200           PERFORM BA-KOLLA-FLER-RADER                                    
042300         END-IF                                                           
042400       ELSE                                                               
042500         IF RAD-IX > 1                                                    
042600           CONTINUE                                                       
042700         ELSE                                                             
042800           MOVE FEL-2(IX) TO MOD-TEMFSFEL                                 
042900         END-IF                                                           
043000         MOVE SPACE TO WS-BEART-SPAR                                      
043100         MOVE ZERO TO WS-IDBENNR-SPAR                                     
043200         MOVE MAX-RADER-PLUS-1 TO RAD-IX                                  
043300       END-IF                                                             
043400     END-PERFORM                                                          
043500     .                                                                    
043600     EJECT                                                                
043700 BA-KOLLA-FLER-RADER SECTION.                                             
043800     SKIP2                                                                
043900     PERFORM IMS-GET-BENA01-ASEQ                                          
044000     IF SEGMENT-FINNS                                                     
044100       MOVE BENA-BEN-IDBENNR TO WS-IDBENNR-SPAR                           
044200       PERFORM IMS-GET-BENA11-SOEK                                        
044300       IF SEGMENT-FINNS                                                   
044400         MOVE BENA-TEXT-BEART TO WS-BEART-SPAR                            
044500         MOVE MED-1(IX) TO MOD-TEMFSINF                                   
044600       ELSE                                                               
044700         MOVE SPACE TO WS-BEART-SPAR                                      
044800         MOVE ZERO TO WS-IDBENNR-SPAR                                     
044900       END-IF                                                             
045000     ELSE                                                                 
045100       MOVE SPACE TO WS-BEART-SPAR                                        
045200       MOVE ZERO TO WS-IDBENNR-SPAR                                       
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600* IMS SEKTIONER                                                           
045700     SKIP3                                                                
045800 IMS-GET-MSG SECTION.                                                     
045900     MOVE '  QC' TO GODK-STATUSKODER                                      
046000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
046100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     SKIP3                                                                
046500 IMS-INSERT-MSG SECTION.                                                  
046600     IF NOT ENGLISH-TEXT                                                  
046700       MOVE '0' TO MFS-KDHUVOMR                                           
046800     END-IF                                                               
046900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
047000     MOVE SPACE TO GODK-STATUSKODER                                       
047100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
047200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
047300     PERFORM IMS-STATUSKONTROLL                                           
047400     .                                                                    
047500     EJECT                                                                
047600 IMS-GET-BENA01-ASEQ SECTION.                                             
047700     STRING 'WLBENA01(WDD3ASEQ=>' W-IDSKYLT                               
047800                      W-BEART-X                                           
047900                     '&WDD3ASEQ <' W-IDSKYLT                              
048000                      W-BEART-MAX-X  ')'                                  
048100            DELIMITED BY SIZE INTO SSA1                                   
048200     MOVE '  GE' TO GODK-STATUSKODER                                      
048300     CALL CBLTDLI USING GN BENA-PCB DLI-IO-AREA-1 SSA1                    
048400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
048500     PERFORM IMS-STATUSKONTROLL                                           
048600     .                                                                    
048700     SKIP3                                                                
048800 IMS-GET-BENA11-SOEK SECTION.                                             
048900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-SOEK-X ')'                    
049000            DELIMITED BY SIZE INTO SSA1                                   
049100     MOVE '  GE' TO GODK-STATUSKODER                                      
049200     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-2 SSA1                    
049300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
049400     PERFORM IMS-STATUSKONTROLL                                           
049500     .                                                                    
049600     SKIP3                                                                
049700 IMS-GET-BENA11-SVAR SECTION.                                             
049800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-SVAR-X ')'                    
049900            DELIMITED BY SIZE INTO SSA1                                   
050000     MOVE '  ' TO GODK-STATUSKODER                                        
050100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-2 SSA1                    
050200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
050300     PERFORM IMS-STATUSKONTROLL                                           
050400     .                                                                    
050500     EJECT                                                                
050600*IMS-GET-BENA11 SECTION.                                                  
050700*    MOVE 'WLBENA11 ' TO SSA1                                             
050800*    MOVE '  GE' TO GODK-STATUSKODER                                      
050900*    CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-2 SSA1                   
051000*    MOVE BENA-STATUS-CODE TO STATUS-WS                                   
051100*    PERFORM IMS-STATUSKONTROLL                                           
051200*    .                                                                    
051300*    SKIP3                                                                
051400 IMS-STATUSKONTROLL SECTION.                                              
051500     SET STATUS-IX TO 1                                                   
051600     SEARCH GODK-STATUS                                                   
051610       AT END                                                             
051620         CALL FELLOG                                                      
051700     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
051800     END-SEARCH                                                           
051900     .                                                                    
