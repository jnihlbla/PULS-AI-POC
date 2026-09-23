000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W006PRS1.                                                
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
002600 77  IDPGM                   PIC X(8)    VALUE 'W006PRS1'.                
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
005100                                                                          
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
005400   03  VIMSID                PIC X(8)    VALUE 'VIMSID  '.                
005500   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
005600   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
005700                                                                          
005800     EJECT                                                                
005900 01  FILLER                  PIC X(16)   VALUE 'W006PRT '.                
006000*01  -COPY W006PRT.                                                       
006100                                                                          
006200     EJECT                                                                
006300 01  FILLER                  PIC X(16)   VALUE 'W006PRTY'.                
006400*01  -COPY W006PRTY.                                                      
006500                                                                          
006600     EJECT                                                                
006700 01  FILLER                  PIC X(16)   VALUE 'W006PRAR'.                
006800*01  -COPY W006PRAR.                                                      
006900                                                                          
007000     EJECT                                                                
007100 01  FILLER                  PIC X(16)   VALUE 'WMSPOOL-AREA'.            
007200*01  -COPY WMSGSPOL                                                       
007300                                                                          
007400     EJECT                                                                
007500******************************************************************        
007600*                                                                         
007700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007800*                                                                         
007900 01  IMS-WS.                                                              
008000   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
008100     SKIP3                                                                
008200*                        **** STATUS-KOD FRÅN IMS                         
008300   03  STATUS-WS             PIC XX.                                      
008400     88  SEGMENT-FINNS                   VALUE '  '.                      
008500                                                                          
008600   03  GODK-STATUSKODER.                                                  
008700     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
008800     EJECT                                                                
008900*                            IMS FUNKTIONSKODER                           
009000*01    -COPY W0003                                                        
009100                                                                          
009200     EJECT                                                                
009300 LINKAGE SECTION.                                                         
009400                                                                          
009500 01  -COPY W006PRTY -PRE LINK-                                            
009600     EJECT                                                                
009700 01  LINK-CALL-TYP           PIC X(5).                                    
009800     SKIP2                                                                
009900 01  LINK-IDPRTLST           PIC X(8).                                    
010000     SKIP2                                                                
010100*01  -COPY W0009 -PRE ALT-.                                               
010200     SKIP2                                                                
010300 01  LINK-SKIP               PIC S9(3) COMP-3.                            
010400     SKIP2                                                                
010500 01  LINK-RAD                PIC X(132).                                  
010600     EJECT                                                                
010700 PROCEDURE DIVISION USING  LINK-PRTY-W006PRTY                             
010800                           LINK-CALL-TYP                                  
010900                           LINK-IDPRTLST                                  
011000                           ALT-PCB                                        
011100                           LINK-SKIP                                      
011200                           LINK-RAD.                                      
011300 STYR SECTION.                                                            
011400                                                                          
011500     EVALUATE LINK-CALL-TYP                                               
011600       WHEN PRT-OPEN  MOVE HIGH-VALUE TO W-IDPRTLST                       
011700                                                                          
011800       WHEN PRT-WRITE PERFORM B-WRITE                                     
011900                                                                          
012000       WHEN PRT-PURGE PERFORM S03-SKRIV                                   
012100                      IF W-FAX-SW = JA                                    
012200                        PERFORM IMS-INSERT-SPOOL-FAX                      
012300                        MOVE NEJ TO W-FAX-SW                              
012400                      END-IF                                              
012500                      MOVE JA TO W-CHANGE-SW                              
012600                                                                          
012700       WHEN PRT-CLOSE PERFORM S03-SKRIV                                   
012800                      IF W-FAX-SW = JA                                    
012900                        PERFORM IMS-INSERT-SPOOL-FAX                      
013000                        MOVE NEJ TO W-FAX-SW                              
013100                      END-IF                                              
013200                      MOVE JA TO W-CHANGE-SW                              
013300     END-EVALUATE                                                         
013400                                                                          
013500     MOVE ZERO TO RETURN-CODE                                             
013600     GOBACK                                                               
013700     .                                                                    
013800     EJECT                                                                
013900 B-WRITE SECTION.                                                         
014000*                         LINK-SKIP > 900 = NYSIDA                        
014100     MOVE LINK-SKIP TO W-SKIP                                             
014200     IF LINK-SKIP > W-NYSIDA                                              
014300       PERFORM S03-SKRIV                                                  
014400       IF     LINK-IDPRTLST      NOT = W-IDPRTLST                         
014500           OR LINK-PRTY-KDCOPIES NOT = PRTY-KDCOPIES                      
014600           OR LINK-PRTY-IDTFX    NOT = PRTY-IDTFX                         
014700         IF W-FAX-SW = JA                                                 
014800           PERFORM IMS-INSERT-SPOOL-FAX                                   
014900           MOVE NEJ TO W-FAX-SW                                           
015000         END-IF                                                           
015100         PERFORM BA-NYPRINTER                                             
015200         MOVE JA TO W-CHANGE-SW                                           
015300       END-IF                                                             
015400       PERFORM S01-TAB-INIT                                               
015500     ELSE                                                                 
015600       IF LINK-SKIP > +800                                                
015700         MOVE W-SKIP TO W-IX                                              
015800       ELSE                                                               
015900         ADD LINK-SKIP TO W-IX                                            
016000       END-IF                                                             
016100       IF W-IX > W-MAX-IX                                                 
016200         MOVE W-MAX-IX TO W-IX                                            
016300         PERFORM S03-SKRIV                                                
016400         PERFORM S01-TAB-INIT                                             
016500       END-IF                                                             
016600     END-IF                                                               
016700     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
016800       MOVE LINK-RAD TO SPOOL-A4S-DATA (W-IX)                             
016900     ELSE                                                                 
017000       MOVE LINK-RAD TO SPOOL-RAD-DATA (W-IX)                             
017100     END-IF                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 BA-NYPRINTER SECTION.                                                    
017500                                                                          
017600     CALL VIMSID USING W-VIMSID                                           
017700     MOVE +900 TO W-NYSIDA                                                
017800     MOVE LINK-IDPRTLST TO PRT-IDPRTLST                                   
017900                           W-IDPRTLST                                     
018000     MOVE '003' TO PRT-KDCALL                                             
018100     CALL W006PRT USING PRT-W006PRT                                       
018200     MOVE PRT-BEPRTLST TO W-PRTTYP                                        
018300     MOVE LINK-PRTY-W006PRTY TO PRTY-W006PRTY                             
018400     IF PRTY-KDCOPIES NUMERIC                                             
018500       MOVE PRTY-KDCOPIES TO SPOOL-COPIES                                 
018600     ELSE                                                                 
018700       MOVE '1'      TO SPOOL-COPIES                                      
018800     END-IF                                                               
018900     EVALUATE PRTY-KDFORMS                                                
019000       WHEN 'B'       MOVE 'F001' TO SPOOL-FORMS                          
019100       WHEN 'C'       MOVE '4811' TO SPOOL-FORMS                          
019200       WHEN 'D'       MOVE 'VCAS' TO SPOOL-FORMS                          
019300       WHEN '2'       MOVE '2000' TO SPOOL-FORMS                          
019400       WHEN '3'       MOVE '3000' TO SPOOL-FORMS                          
019500       WHEN OTHER     MOVE 'STD ' TO SPOOL-FORMS                          
019600     END-EVALUATE                                                         
019700     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
019800        MOVE +85 TO W-RADL                                                
019900        MOVE +80 TO W-MAX-IX                                              
020000     ELSE                                                                 
020100       MOVE +137 TO W-RADL                                                
020200       MOVE +70 TO W-MAX-IX                                               
020300     END-IF                                                               
020400     IF W-PRTTYP = 'LASER' OR 'TERMO' OR 'ZEBRA'                          
020410     OR W-PRTTYP (1:4) = 'NOVA'                                           
020500       MOVE 'IAFP=N1M,PRTO=' TO SPOOL-IAFP                                
020600     ELSE                                                                 
020700       MOVE 'IAFP=A1M,PRTO=' TO SPOOL-IAFP                                
020800     END-IF                                                               
020900     MOVE PRT-IDNODE TO SPOOL-IDNODE                                      
020910*    DISPLAY 'PRT-IDNODE: ' PRT-IDNODE                                    
021000     IF W-PRTTYP (1:3) = 'IBM'                                            
021100       IF PRTY-IDPFDEF = SPACE AND W-IDPRTLST (1:1) = 'W'                 
021200         MOVE W-IDPRTLST (1:6) TO PRTY-IDPFDEF                            
021300       END-IF                                                             
021400     END-IF                                                               
021500     IF PRTY-IDPFDEF NOT = SPACE                                          
021600       MOVE PRTY-IDPFDEF TO SPOOL-FORMDEF SPOOL-PAGEDEF                   
021700       MOVE 'A'  TO SPOOL-CLASS                                           
021800*            -- SET TO CLASS E FOR OLD JES2 PRINTERS                      
021900       IF PRT-IDNODE (1:4) = 'NJO0'                                       
022000         MOVE 'E'  TO SPOOL-CLASS                                         
022100       END-IF                                                             
022200                                                                          
022300       MOVE PRT-IDNODE  TO TEST-PRINTER-NAME                              
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
024060         MOVE 'W.IGRT.PSF,W.PROD.PSF)' TO SPOOL-USERLIB                   
024070         MOVE 22 TO SPOOL-USERLIB-LENGTH                                  
024091       END-IF                                                             
024100       MOVE SPOOL-IBM-LASER TO SPOOL-OVR-PARAM                            
024200       COMPUTE SPOOL-IAFP-LL = LENGTH OF SPOOL-NORMAL-OPTIONS             
024300                             + LENGTH OF SPOOL-IBM-LASER-DEL1             
024400                             + SPOOL-USERLIB-LENGTH                       
024500     ELSE                                                                 
024600       COMPUTE SPOOL-IAFP-LL = LENGTH OF SPOOL-NORMAL-OPTIONS             
024700       MOVE 'A'  TO SPOOL-CLASS                                           
024800       IF PRT-IDNODE (1:3) = 'R31' OR 'N38'                               
024900         MOVE 'F001' TO SPOOL-FORMS                                       
025000       ELSE                                                               
025100         IF PRT-IDNODE = 'R3200371'                                       
025200           MOVE '4811' TO SPOOL-FORMS                                     
025300         END-IF                                                           
025400       END-IF                                                             
025500     END-IF                                                               
025600                                                                          
025700     IF W-IMSID = 'IM1P' OR 'IMG0'                                        
025800       CONTINUE                                                           
025900     ELSE                                                                 
026000       IF PRT-IDNODE = 'QSE09012' OR 'QSE09013'                           
026100                    OR 'NJOV1   ' OR 'NJOV2   '                           
026200         MOVE 'IAFP=A00,PRTO=' TO SPOOL-IAFP                              
026300         MOVE 'H' TO SPOOL-CLASS                                          
026400         MOVE ',OUTDISP(HOLD,HOLD)' TO SPOOL-OVR-PARAM1                   
026500         MOVE SPOOL-IBM-LASER       TO SPOOL-OVR-PARAM2                   
026600         COMPUTE SPOOL-IAFP-LL = SPOOL-IAFP-LL + 19                       
026700       END-IF                                                             
026800     END-IF                                                               
026900                                                                          
027000     COMPUTE SPOOL-OPT-LL  = SPOOL-IAFP-LL                                
027100                           + LENGTH OF SPOOL-START-OPTIONS                
027200     .                                                                    
027300     EJECT                                                                
027400 S01-TAB-INIT SECTION.                                                    
027500                                                                          
027600     MOVE +1 TO INDX                                                      
027700     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
027800       PERFORM 80 TIMES                                                   
027900         MOVE +85   TO SPOOL-A4S-RDW (INDX)                               
028000         MOVE ZERO  TO SPOOL-A4S-ZZ  (INDX)                               
028100         MOVE SPACE TO SPOOL-A4S     (INDX)                               
028200         ADD +1 TO INDX                                                   
028300       END-PERFORM                                                        
028400       MOVE '1' TO SPOOL-A4S-STYR (1)                                     
028500     ELSE                                                                 
028600       PERFORM 70 TIMES                                                   
028700         MOVE +137  TO SPOOL-RAD-RDW (INDX)                               
028800         MOVE ZERO  TO SPOOL-RAD-ZZ  (INDX)                               
028900         MOVE SPACE TO SPOOL-RAD     (INDX)                               
029000         ADD +1 TO INDX                                                   
029100       END-PERFORM                                                        
029200       MOVE '1' TO SPOOL-RAD-STYR (1)                                     
029300     END-IF                                                               
029400                                                                          
029500     IF W-SKIP > W-MAX-IX                                                 
029600       MOVE +1 TO W-IX                                                    
029700     ELSE                                                                 
029800       MOVE W-SKIP TO W-IX                                                
029900       IF (W-PRTTYP = 'IBMLB' OR 'IBMLD')                                 
030000       AND  (PRTY-IDPFDEF(1:4) = 'W475')                                  
030100         ADD +1 TO W-IX                                                   
030200       END-IF                                                             
030300     END-IF                                                               
030400     MOVE JA TO W-SKRIV-SW                                                
030500     .                                                                    
030600     EJECT                                                                
030700 S03-SKRIV SECTION.                                                       
030800                                                                          
030900     IF W-SKRIV-SW = JA                                                   
031000       COMPUTE SPOOL-RAD-BDW = W-RADL * W-IX + 4                          
031100       IF PRT-IDNODE = 'QSE09012' OR 'QSE09013'                           
031200         CONTINUE                                                         
031300       ELSE                                                               
031400         IF W-PRTTYP = 'LASER' OR 'TERMO' OR 'ZEBRA'                      
031410         OR W-PRTTYP (1:4) = 'NOVA'                                       
031500*                    STYRTECKEN TAS BORT FRÅN POS 1 I ALLA RADER          
031600           MOVE +1 TO INDX                                                
031700           IF PRTY-IDPRTSPO = 'SPO-A4S '                                  
031800             PERFORM 80 TIMES                                             
031900               MOVE SPOOL-A4S-DATA (INDX) TO SPOOL-A4S (INDX)             
032000               ADD +1 TO INDX                                             
032100             END-PERFORM                                                  
032200           ELSE                                                           
032300             PERFORM 70 TIMES                                             
032400               MOVE SPOOL-RAD-DATA (INDX) TO SPOOL-RAD (INDX)             
032500               ADD +1 TO INDX                                             
032600             END-PERFORM                                                  
032700           END-IF                                                         
032800         END-IF                                                           
032900       END-IF                                                             
033000       INSPECT SPOOL-RAD-AREA REPLACING ALL '*COPY*' BY SPACE             
033100       IF W-PRTTYP = 'NOPRT'                                              
033200         CONTINUE                                                         
033300       ELSE                                                               
033400         IF W-CHANGE-SW = JA                                              
033500           IF PRTY-IDTFX NOT = SPACE                                      
033600               AND PRT-IDNODE (1:3) = 'FAX'                               
033700             MOVE 'QSERFAX ' TO SPOOL-IDNODE                              
033800             MOVE 'A'  TO SPOOL-CLASS                                     
033900             MOVE '1'  TO SPOOL-COPIES                                    
034000             MOVE +65  TO SPOOL-FAX-RDW  (1)                              
034100             MOVE ZERO TO SPOOL-FAX-ZZ   (1)                              
034200             MOVE '1'  TO SPOOL-FAX-STYR (1)                              
034300             EVALUATE W-IMSID                                             
034400               WHEN 'IMG0'                                                
034500               WHEN 'IM1P'                                                
034600                 MOVE SPACE TO SPOOL-FAX-DATA (1)                         
034700                 STRING '*AUTOFAX*<TOFAXNUM:' PRTY-IDTFX                  
034800                     DELIMITED BY SIZE INTO SPOOL-FAX-DATA (1)            
034900                 INSPECT SPOOL-FAX-DATA (1)                               
035000                     REPLACING FIRST SPACE BY '>'                         
035100               WHEN OTHER                                                 
035200                 MOVE '*AUTOFAX* <TOFAXNUM:62612>'                        
035300                     TO SPOOL-FAX-DATA (1)                                
035400             END-EVALUATE                                                 
035500             MOVE +65  TO SPOOL-FAX-RDW  (2)                              
035600             MOVE ZERO TO SPOOL-FAX-ZZ   (2)                              
035700             MOVE ' '  TO SPOOL-FAX-STYR (2)                              
035800             MOVE '<FROMNAME:PULS FAX DISTRIBUTION SYSTEM>'               
035900                  TO SPOOL-FAX-DATA (2)                                   
036000             MOVE +65  TO SPOOL-FAX-RDW  (3)                              
036100             MOVE ZERO TO SPOOL-FAX-ZZ   (3)                              
036200             MOVE ' '  TO SPOOL-FAX-STYR (3)                              
036300             MOVE '<TONAME:FAX RECEIVER>'                                 
036400                  TO SPOOL-FAX-DATA (3)                                   
036500             MOVE +1 TO INDX                                              
036600             PERFORM 5 TIMES                                              
036700               MOVE +65  TO SPOOL-FAX-RDW  (INDX + 3)                     
036800               MOVE ZERO TO SPOOL-FAX-ZZ   (INDX + 3)                     
036900               MOVE ' '  TO SPOOL-FAX-STYR (INDX + 3)                     
037000               STRING '<NOTE:' PRTY-TEFAX (INDX) '>   '                   
037100                 DELIMITED BY SIZE INTO SPOOL-FAX-DATA (INDX + 3)         
037200               ADD +1 TO INDX                                             
037300             END-PERFORM                                                  
037400             MOVE +65  TO SPOOL-FAX-RDW  (9)                              
037500             MOVE ZERO TO SPOOL-FAX-ZZ   (9)                              
037600             MOVE ' '  TO SPOOL-FAX-STYR (9)                              
037700             MOVE '<DELETELASTPAGE>'                                      
037800                  TO SPOOL-FAX-DATA (9)                                   
037900             MOVE JA TO W-FAX-SW                                          
038000           END-IF                                                         
038100           PERFORM IMS-CHANGE-SPOOL                                       
038200         END-IF                                                           
038300         PERFORM IMS-INSERT-SPOOL                                         
038400       END-IF                                                             
038500       MOVE NEJ TO W-SKRIV-SW                                             
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900* IMS SEKTIONER                                                           
039000                                                                          
039100 IMS-CHANGE-SPOOL SECTION.                                                
039200                                                                          
039300     IF W-PRTTYP = 'NOPRT'                                                
039400       CONTINUE                                                           
039500     ELSE                                                                 
039600       MOVE SPACE TO GODK-STATUSKODER                                     
039700       CALL CBLTDLI USING CHNG ALT-PCB SPOOL-OPT-DEST                     
039800                               SPOOL-OPTIONS SPOOL-FEEDBACK               
039900       MOVE ALT-STATUS-CODE TO STATUS-WS                                  
040000       PERFORM IMS-STATUSKONTROLL-SPOOL                                   
040100       MOVE NEJ TO W-CHANGE-SW                                            
040200     END-IF                                                               
040300     .                                                                    
040400                                                                          
040500                                                                          
040600 IMS-INSERT-SPOOL SECTION.                                                
040700                                                                          
040800     MOVE SPACE TO GODK-STATUSKODER                                       
040900     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-RAD-AREA                       
041000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
041100     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
041200     .                                                                    
041300                                                                          
041400                                                                          
041500 IMS-INSERT-SPOOL-FAX SECTION.                                            
041600                                                                          
041700     MOVE SPACE TO GODK-STATUSKODER                                       
041800     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-FAX-AREA                       
041900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
042000     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
042100     .                                                                    
042200                                                                          
042300                                                                          
042400     EJECT                                                                
042500                                                                          
042600 IMS-STATUSKONTROLL-SPOOL SECTION.                                        
042700                                                                          
042800     SET STATUS-IX TO 1                                                   
042900     SEARCH GODK-STATUS                                                   
043000       AT END                                                             
043100         MOVE 'FEL FRÅN IMS SPOOL-INTERFACE SE SPOOL-FEEDBACK'            
043200           TO FELTEXT                                                     
043300         CALL FELLOG                                                      
043400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
043500         CONTINUE                                                         
043600     END-SEARCH                                                           
043700     .                                                                    
