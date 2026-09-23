000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W006PRS2.                                                
000400 AUTHOR.         RICHARD.                                                 
000500 DATE-WRITTEN.   AUG.  93.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        GENERELLT LISTNINGSPROGRAM                                       
001000*        VIA SPOOL-API UTAN ÅTERSTART                                     
001100*        TILL PRINTER ELLER FAX                                           
001200*                                                                         
001300*    INDATA.                                                              
001400*        CALL-PARAMETRAR FRÅN KALLANDE PROGRAM.                           
001500*                                                                         
001600*    UTDATA.                                                              
001700*        SIDOR TILL SPOOL-API                                             
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                   PIC X(8)    VALUE 'W006PRS2'.                
002700 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
002800 77  W-MAX-IX                PIC S9(9)   VALUE +1    COMP.                
002900 77  INDX                    PIC S9(9)   VALUE +1    COMP.                
003000 77  W-IX                    PIC S9(9)   VALUE +1    COMP.                
003100 77  W-IX-SPAR               PIC S9(9)   VALUE +0    COMP.                
003200 77  W-RADL                  PIC S9(4)   VALUE +137  COMP.                
003300 77  W-NYSIDA                PIC S9(3)   VALUE +0    COMP-3.              
003400 77  W-SKIP                  PIC S9(2)   VALUE +1    COMP-3.              
003500 77  JA                      PIC X       VALUE 'J'.                       
003600 77  NEJ                     PIC X       VALUE 'N'.                       
003700 77  W-SKRIV-SW              PIC X       VALUE 'N'.                       
003800 77  W-FAX-SW                PIC X       VALUE 'N'.                       
003900 77  W-CHANGE-SW             PIC X       VALUE 'J'.                       
004000 77  W-IDPRTLST              PIC X(8)    VALUE HIGH-VALUE.                
004100 77  W-PRTTYP                PIC X(5)    VALUE SPACE.                     
004200                                                                          
004300 01  W-VIMSID.                                                            
004400   03  W-IMSID               PIC X(4)    VALUE SPACE.                     
004500   03                        PIC X(4)    VALUE SPACE.                     
004600                                                                          
004700 01  TEST-PRINTER-NAME       PIC X(8).                                    
004710* V6+VT PRINTRAR UTGÅTT. LÄMNAR KVAR VX FÖR EV FRAMTIDA BEHOV.            
004720 88  VX-PRINTER              VALUE 'QSE00000'.                            
005200                                                                          
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
005500   03  VIMSID                PIC X(8)    VALUE 'VIMSID  '.                
005600   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
005700   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
005800                                                                          
005900     EJECT                                                                
006000 01  FILLER                  PIC X(16)   VALUE 'W006PRT '.                
006100*01  -COPY W006PRT.                                                       
006200                                                                          
006300     EJECT                                                                
006400 01  FILLER                  PIC X(16)   VALUE 'W006PRTY'.                
006500*01  -COPY W006PRTY.                                                      
006600                                                                          
006700     EJECT                                                                
006800 01  FILLER                  PIC X(16)   VALUE 'W006PRAR'.                
006900*01  -COPY W006PRAR.                                                      
007000                                                                          
007100     EJECT                                                                
007200 01  FILLER                  PIC X(16)   VALUE 'SPOOL-AREA'.              
007300*01  -COPY WMSGSPOL                                                       
007400                                                                          
007500     EJECT                                                                
007600******************************************************************        
007700*                                                                         
007800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007900*                                                                         
008000 01  IMS-WS.                                                              
008100   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
008200     SKIP3                                                                
008300*                        **** STATUS-KOD FRÅN IMS                         
008400   03  STATUS-WS             PIC XX.                                      
008500     88  SEGMENT-FINNS                   VALUE '  '.                      
008600                                                                          
008700   03  GODK-STATUSKODER.                                                  
008800     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
008900     EJECT                                                                
009000*                            IMS FUNKTIONSKODER                           
009100*01    -COPY W0003                                                        
009200                                                                          
009300     EJECT                                                                
009400 LINKAGE SECTION.                                                         
009500                                                                          
009600 01  -COPY W006PRTY -PRE LINK-                                            
009700     EJECT                                                                
009800 01  LINK-CALL-TYP           PIC X(5).                                    
009900     SKIP2                                                                
010000 01  LINK-IDPRTLST           PIC X(8).                                    
010100     SKIP2                                                                
010200*01  -COPY W0009 -PRE ALT-.                                               
010300     SKIP2                                                                
010400 01  LINK-SKIP               PIC S9(3) COMP-3.                            
010500     SKIP2                                                                
010600 01  LINK-RAD                PIC X(132).                                  
010700     EJECT                                                                
010800 PROCEDURE DIVISION USING  LINK-PRTY-W006PRTY                             
010900                           LINK-CALL-TYP                                  
011000                           LINK-IDPRTLST                                  
011100                           ALT-PCB                                        
011200                           LINK-SKIP                                      
011300                           LINK-RAD.                                      
011400 STYR SECTION.                                                            
011500                                                                          
011600     EVALUATE LINK-CALL-TYP                                               
011700       WHEN PRT-OPEN  MOVE HIGH-VALUE TO W-IDPRTLST                       
011800                                                                          
011900       WHEN PRT-WRITE PERFORM B-WRITE                                     
012000                                                                          
012100       WHEN PRT-PURGE PERFORM S03-SKRIV                                   
012200                      IF W-FAX-SW = JA                                    
012300                        PERFORM IMS-INSERT-SPOOL-FAX                      
012400                        MOVE NEJ TO W-FAX-SW                              
012500                      END-IF                                              
012600                      MOVE JA TO W-CHANGE-SW                              
012700                                                                          
012800       WHEN PRT-CLOSE PERFORM S03-SKRIV                                   
012900                      IF W-FAX-SW = JA                                    
013000                        PERFORM IMS-INSERT-SPOOL-FAX                      
013100                        MOVE NEJ TO W-FAX-SW                              
013200                      END-IF                                              
013300                      MOVE JA TO W-CHANGE-SW                              
013400     END-EVALUATE                                                         
013500                                                                          
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 B-WRITE SECTION.                                                         
014100*                         LINK-SKIP > 900 = NYSIDA                        
014200     MOVE LINK-SKIP TO W-SKIP                                             
014300     IF LINK-SKIP > W-NYSIDA                                              
014400       PERFORM S03-SKRIV                                                  
014500       IF     LINK-IDPRTLST      NOT = W-IDPRTLST                         
014600           OR LINK-PRTY-KDCOPIES NOT = PRTY-KDCOPIES                      
014700           OR LINK-PRTY-IDTFX    NOT = PRTY-IDTFX                         
014800         IF W-FAX-SW = JA                                                 
014900           PERFORM IMS-INSERT-SPOOL-FAX                                   
015000           MOVE NEJ TO W-FAX-SW                                           
015100         END-IF                                                           
015200         PERFORM BA-NYPRINTER                                             
015300         MOVE JA TO W-CHANGE-SW                                           
015400       END-IF                                                             
015500       PERFORM S01-TAB-INIT                                               
015600     ELSE                                                                 
015700       IF LINK-SKIP > +800                                                
015800         MOVE W-SKIP TO W-IX                                              
015900       ELSE                                                               
016000         ADD LINK-SKIP TO W-IX                                            
016100       END-IF                                                             
016200       IF W-IX > W-MAX-IX                                                 
016300         MOVE W-MAX-IX TO W-IX                                            
016400         PERFORM S03-SKRIV                                                
016500         PERFORM S01-TAB-INIT                                             
016600       END-IF                                                             
016700     END-IF                                                               
016800     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
016900       MOVE LINK-RAD TO SPOOL-A4S-DATA (W-IX)                             
017000     ELSE                                                                 
017100       MOVE LINK-RAD TO SPOOL-RAD-DATA (W-IX)                             
017200     END-IF                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 BA-NYPRINTER SECTION.                                                    
017600                                                                          
017700     CALL VIMSID USING W-VIMSID                                           
017800     MOVE +900 TO W-NYSIDA                                                
017900     MOVE LINK-IDPRTLST TO PRT-IDPRTLST                                   
018000                           W-IDPRTLST                                     
018100     MOVE '003' TO PRT-KDCALL                                             
018200     CALL W006PRT USING PRT-W006PRT                                       
018300     MOVE PRT-BEPRTLST TO W-PRTTYP                                        
018400     MOVE LINK-PRTY-W006PRTY TO PRTY-W006PRTY                             
018500     IF PRTY-KDCOPIES NUMERIC                                             
018600       MOVE PRTY-KDCOPIES TO SPOOL-COPIES                                 
018700     ELSE                                                                 
018800       MOVE '1'      TO SPOOL-COPIES                                      
018900     END-IF                                                               
019000     EVALUATE PRTY-KDFORMS                                                
019100       WHEN 'B'       MOVE 'F001' TO SPOOL-FORMS                          
019200       WHEN 'C'       MOVE '4811' TO SPOOL-FORMS                          
019300       WHEN 'D'       MOVE 'VCAS' TO SPOOL-FORMS                          
019400       WHEN '2'       MOVE '2000' TO SPOOL-FORMS                          
019500       WHEN '3'       MOVE '3000' TO SPOOL-FORMS                          
019600       WHEN OTHER     MOVE 'STD ' TO SPOOL-FORMS                          
019700     END-EVALUATE                                                         
019800     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
019900        MOVE +85 TO W-RADL                                                
020000        MOVE +80 TO W-MAX-IX                                              
020100     ELSE                                                                 
020200       MOVE +137 TO W-RADL                                                
020300       MOVE +70 TO W-MAX-IX                                               
020400     END-IF                                                               
020500     IF W-PRTTYP = 'LASER' OR 'TERMO' OR 'ZEBRA'                          
020600       MOVE 'IAFP=N1M,PRTO=' TO SPOOL-IAFP                                
020700     ELSE                                                                 
020800       MOVE 'IAFP=A1M,PRTO=' TO SPOOL-IAFP                                
020900     END-IF                                                               
021000     MOVE PRT-IDNODE TO SPOOL-IDNODE                                      
021100     IF W-PRTTYP (1:3) = 'IBM'                                            
021200       IF PRTY-IDPFDEF = SPACE AND W-IDPRTLST (1:1) = 'W'                 
021300         MOVE W-IDPRTLST (1:6) TO PRTY-IDPFDEF                            
021400       END-IF                                                             
021500     END-IF                                                               
021600     IF PRTY-IDPFDEF NOT = SPACE                                          
021700       MOVE PRTY-IDPFDEF TO SPOOL-FORMDEF SPOOL-PAGEDEF                   
021800       MOVE 'A'  TO SPOOL-CLASS                                           
021900*            -- SET TO CLASS E FOR OLD JES2 PRINTERS                      
022000       IF PRT-IDNODE (1:4) = 'NJO0'                                       
022100         MOVE 'E'  TO SPOOL-CLASS                                         
022200       END-IF                                                             
022300                                                                          
022400       IF W-IMSID = 'IMG0'                                                
022410         IF VX-PRINTER                                                    
022420* FÖR EV FRAMTIDA BEHOV                                                   
022430           MOVE 'W??.????.PSF)' TO SPOOL-USERLIB                          
022440           MOVE 13 TO SPOOL-USERLIB-LENGTH                                
022450         ELSE                                                             
022460           MOVE 'W.QASE.PSF)' TO SPOOL-USERLIB                            
022470           MOVE 11 TO SPOOL-USERLIB-LENGTH                                
022480         END-IF                                                           
023700       ELSE                                                               
024600         MOVE 'W.IGRT.PSF,W.PROD.PSF)' TO SPOOL-USERLIB                   
024700         MOVE 22 TO SPOOL-USERLIB-LENGTH                                  
025000       END-IF                                                             
025100       MOVE PRT-IDNODE  TO TEST-PRINTER-NAME                              
025200       MOVE SPOOL-IBM-LASER TO SPOOL-OVR-PARAM                            
025300       COMPUTE SPOOL-IAFP-LL = LENGTH OF SPOOL-NORMAL-OPTIONS             
025400                             + LENGTH OF SPOOL-IBM-LASER-DEL1             
025500                             + SPOOL-USERLIB-LENGTH                       
025600     ELSE                                                                 
025700       COMPUTE SPOOL-IAFP-LL = LENGTH OF SPOOL-NORMAL-OPTIONS             
025800       MOVE 'A'  TO SPOOL-CLASS                                           
025900       IF PRT-IDNODE (1:3) = 'R31' OR 'N38'                               
026000         MOVE 'F001' TO SPOOL-FORMS                                       
026100       ELSE                                                               
026200         IF PRT-IDNODE = 'R3200371'                                       
026300           MOVE '4811' TO SPOOL-FORMS                                     
026400         END-IF                                                           
026500       END-IF                                                             
026600     END-IF                                                               
026700                                                                          
026800     IF W-IMSID = 'IM1P' OR 'IMG0'                                        
026900       CONTINUE                                                           
027000     ELSE                                                                 
027100       IF PRT-IDNODE = 'QSE09012' OR 'QSE09013'                           
027200                    OR 'NJOV1   ' OR 'NJOV2   '                           
027300         MOVE 'IAFP=A00,PRTO=' TO SPOOL-IAFP                              
027400         MOVE 'H' TO SPOOL-CLASS                                          
027500         MOVE ',OUTDISP(HOLD,HOLD)' TO SPOOL-OVR-PARAM1                   
027600         MOVE SPOOL-IBM-LASER       TO SPOOL-OVR-PARAM2                   
027700         COMPUTE SPOOL-IAFP-LL = SPOOL-IAFP-LL + 19                       
027800       END-IF                                                             
027900     END-IF                                                               
028000                                                                          
028100     COMPUTE SPOOL-OPT-LL  = SPOOL-IAFP-LL                                
028200                           + LENGTH OF SPOOL-START-OPTIONS                
028300     .                                                                    
028400     EJECT                                                                
028500 S01-TAB-INIT SECTION.                                                    
028600                                                                          
028700     MOVE +1 TO INDX                                                      
028800     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
028900       PERFORM 80 TIMES                                                   
029000         MOVE +85   TO SPOOL-A4S-RDW (INDX)                               
029100         MOVE ZERO  TO SPOOL-A4S-ZZ  (INDX)                               
029200         MOVE SPACE TO SPOOL-A4S     (INDX)                               
029300         ADD +1 TO INDX                                                   
029400       END-PERFORM                                                        
029500       MOVE '1' TO SPOOL-A4S-STYR (1)                                     
029600     ELSE                                                                 
029700       PERFORM 70 TIMES                                                   
029800         MOVE +137  TO SPOOL-RAD-RDW (INDX)                               
029900         MOVE ZERO  TO SPOOL-RAD-ZZ  (INDX)                               
030000         MOVE SPACE TO SPOOL-RAD     (INDX)                               
030100         ADD +1 TO INDX                                                   
030200       END-PERFORM                                                        
030300       MOVE '1' TO SPOOL-RAD-STYR (1)                                     
030400     END-IF                                                               
030500                                                                          
030600     IF W-SKIP > W-MAX-IX                                                 
030700       MOVE +1 TO W-IX                                                    
030800     ELSE                                                                 
030900       MOVE W-SKIP TO W-IX                                                
031000       IF W-PRTTYP = 'IBMLB' OR 'IBMLD'                                   
031100       AND  (PRTY-IDPFDEF(1:4) = 'W475')                                  
031200         ADD +1 TO W-IX                                                   
031300       END-IF                                                             
031400     END-IF                                                               
031500     MOVE JA TO W-SKRIV-SW                                                
031600     .                                                                    
031700     EJECT                                                                
031800 S03-SKRIV SECTION.                                                       
031900                                                                          
032000     IF W-SKRIV-SW = JA                                                   
032100       COMPUTE SPOOL-RAD-BDW = W-RADL * W-IX + 4                          
032200       IF PRT-IDNODE = 'QSE09012' OR 'QSE09013'                           
032300         CONTINUE                                                         
032400       ELSE                                                               
032500         IF W-PRTTYP = 'LASER' OR 'TERMO' OR 'ZEBRA'                      
032600*                    STYRTECKEN TAS BORT FRÅN POS 1 I ALLA RADER          
032700           MOVE +1 TO INDX                                                
032800           IF PRTY-IDPRTSPO = 'SPO-A4S '                                  
032900             PERFORM 80 TIMES                                             
033000               MOVE SPOOL-A4S-DATA (INDX) TO SPOOL-A4S (INDX)             
033100               ADD +1 TO INDX                                             
033200             END-PERFORM                                                  
033300           ELSE                                                           
033400             PERFORM 70 TIMES                                             
033500               MOVE SPOOL-RAD-DATA (INDX) TO SPOOL-RAD (INDX)             
033600               ADD +1 TO INDX                                             
033700             END-PERFORM                                                  
033800           END-IF                                                         
033900         END-IF                                                           
034000       END-IF                                                             
034100       INSPECT SPOOL-RAD-AREA REPLACING ALL '*COPY*' BY SPACE             
034200       IF W-PRTTYP = 'NOPRT'                                              
034300         CONTINUE                                                         
034400       ELSE                                                               
034500         IF W-CHANGE-SW = JA                                              
034600           IF PRTY-IDTFX NOT = SPACE                                      
034700               AND PRT-IDNODE (1:3) = 'FAX'                               
034800             MOVE 'QSERFAX ' TO SPOOL-IDNODE                              
034900             MOVE 'A'  TO SPOOL-CLASS                                     
035000             MOVE '1'  TO SPOOL-COPIES                                    
035100             MOVE +65  TO SPOOL-FAX-RDW  (1)                              
035200             MOVE ZERO TO SPOOL-FAX-ZZ   (1)                              
035300             MOVE '1'  TO SPOOL-FAX-STYR (1)                              
035400             EVALUATE W-IMSID                                             
035500               WHEN 'IMG0'                                                
035600               WHEN 'IM1P'                                                
035700                 MOVE SPACE TO SPOOL-FAX-DATA (1)                         
035800                 STRING '*AUTOFAX*<TOFAXNUM:' PRTY-IDTFX                  
035900                     DELIMITED BY SIZE INTO SPOOL-FAX-DATA (1)            
036000                 INSPECT SPOOL-FAX-DATA (1)                               
036100                     REPLACING FIRST SPACE BY '>'                         
036200               WHEN OTHER                                                 
036300                 MOVE '*AUTOFAX* <TOFAXNUM:62612>'                        
036400                     TO SPOOL-FAX-DATA (1)                                
036500             END-EVALUATE                                                 
036600             MOVE +65  TO SPOOL-FAX-RDW  (2)                              
036700             MOVE ZERO TO SPOOL-FAX-ZZ   (2)                              
036800             MOVE ' '  TO SPOOL-FAX-STYR (2)                              
036900             MOVE '<FROMNAME:PULS FAX DISTRIBUTION SYSTEM>'               
037000                  TO SPOOL-FAX-DATA (2)                                   
037100             MOVE +65  TO SPOOL-FAX-RDW  (3)                              
037200             MOVE ZERO TO SPOOL-FAX-ZZ   (3)                              
037300             MOVE ' '  TO SPOOL-FAX-STYR (3)                              
037400             MOVE '<TONAME:FAX RECEIVER>'                                 
037500                  TO SPOOL-FAX-DATA (3)                                   
037600             MOVE +1 TO INDX                                              
037700             PERFORM 5 TIMES                                              
037800               MOVE +65  TO SPOOL-FAX-RDW  (INDX + 3)                     
037900               MOVE ZERO TO SPOOL-FAX-ZZ   (INDX + 3)                     
038000               MOVE ' '  TO SPOOL-FAX-STYR (INDX + 3)                     
038100               STRING '<NOTE:' PRTY-TEFAX (INDX) '>   '                   
038200                 DELIMITED BY SIZE INTO SPOOL-FAX-DATA (INDX + 3)         
038300               ADD +1 TO INDX                                             
038400             END-PERFORM                                                  
038500             MOVE +65  TO SPOOL-FAX-RDW  (9)                              
038600             MOVE ZERO TO SPOOL-FAX-ZZ   (9)                              
038700             MOVE ' '  TO SPOOL-FAX-STYR (9)                              
038800             MOVE '<DELETELASTPAGE>'                                      
038900                  TO SPOOL-FAX-DATA (9)                                   
039000             MOVE JA TO W-FAX-SW                                          
039100           END-IF                                                         
039200           PERFORM IMS-CHANGE-SPOOL                                       
039300         END-IF                                                           
039400         PERFORM IMS-INSERT-SPOOL                                         
039500       END-IF                                                             
039600       MOVE NEJ TO W-SKRIV-SW                                             
039700     END-IF                                                               
039800     .                                                                    
039900     EJECT                                                                
040000* IMS SEKTIONER                                                           
040100                                                                          
040200 IMS-CHANGE-SPOOL SECTION.                                                
040300                                                                          
040400     IF W-PRTTYP = 'NOPRT'                                                
040500       CONTINUE                                                           
040600     ELSE                                                                 
040700       MOVE SPACE TO GODK-STATUSKODER                                     
040800       CALL CBLTDLI USING CHNG ALT-PCB SPOOL-OPT-DEST                     
040900                               SPOOL-OPTIONS SPOOL-FEEDBACK               
041000       MOVE ALT-STATUS-CODE TO STATUS-WS                                  
041100       PERFORM IMS-STATUSKONTROLL-SPOOL                                   
041200       MOVE NEJ TO W-CHANGE-SW                                            
041300     END-IF                                                               
041400     .                                                                    
041500                                                                          
041600                                                                          
041700 IMS-INSERT-SPOOL SECTION.                                                
041800                                                                          
041900     MOVE SPACE TO GODK-STATUSKODER                                       
042000     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-RAD-AREA                       
042100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
042200     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
042300     .                                                                    
042400                                                                          
042500                                                                          
042600 IMS-INSERT-SPOOL-FAX SECTION.                                            
042700                                                                          
042800     MOVE SPACE TO GODK-STATUSKODER                                       
042900     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-FAX-AREA                       
043000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
043100     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
043200     .                                                                    
043300                                                                          
043400                                                                          
043500     EJECT                                                                
043600                                                                          
043700 IMS-STATUSKONTROLL-SPOOL SECTION.                                        
043800                                                                          
043900     SET STATUS-IX TO 1                                                   
044000     SEARCH GODK-STATUS                                                   
044100       AT END                                                             
044200         MOVE 'FEL FRÅN IMS SPOOL-INTERFACE SE SPOOL-FEEDBACK'            
044300           TO FELTEXT                                                     
044400         CALL FELLOG                                                      
044500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
044600         CONTINUE                                                         
044700     END-SEARCH                                                           
045000     .                                                                    
