000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W006PRR1.                                                
000400 AUTHOR.         RICHARD.                                                 
000500 DATE-WRITTEN.   AUG.  93.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*        GENERELLT LISTNINGSPROGRAM                                       
001000*        VIA SPOOL-API MED ÅTERSTART                                      
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
002600 77  IDPGM                   PIC X(8)    VALUE 'W006PRR1'.                
002700 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
002800 77  W-MAX-IX                PIC S9(9)   VALUE +1    COMP.                
002900 77  INDX                    PIC S9(9)   VALUE +1    COMP.                
003000 77  INDX2                   PIC S9(9)   VALUE +1    COMP.                
003100 77  W-IX                    PIC S9(9)   VALUE +1    COMP.                
003200 77  W-RADL                  PIC S9(4)   VALUE +137  COMP.                
003300 77  W-NYSIDA                PIC S9(3)   VALUE +0    COMP-3.              
003400 77  W-SKIP                  PIC S9(2)   VALUE +1    COMP-3.              
003500 77  JA                      PIC X       VALUE 'J'.                       
003600 77  NEJ                     PIC X       VALUE 'N'.                       
003700 77  W-SKRIV-SW              PIC X       VALUE 'N'.                       
003800 77  W-FAX-SW                PIC X       VALUE 'N'.                       
003900 77  W-CHANGE-SW             PIC X       VALUE 'J'.                       
004000 77  W-PRT-NYLISTA           PIC X       VALUE 'J'.                       
004100 77  W-IDPRTLST              PIC X(8)    VALUE HIGH-VALUE.                
004200 77  W-IDPRTLST-SPAR         PIC X(8)    VALUE HIGH-VALUE.                
004300 77  W-DATUM                 PIC S9(7)   VALUE ZERO  COMP-3.              
004400 77  W-TIME                  PIC S9(9)   VALUE ZERO  COMP-3.              
004500 77  W-NIOR                  PIC S9(9)   VALUE +999999999 COMP-3.         
004600 77  W-IDSID                 PIC S9(3)   VALUE ZERO  COMP-3.              
004700 77  W-PRTTYP                PIC X(5)    VALUE SPACE.                     
004800                                                                          
004900 01  W-IDENT.                                                             
005000   03  W-IDLIST              PIC X(10)   VALUE HIGH-VALUE.                
005100   03  FILLER                PIC X(10)   VALUE SPACE.                     
005200   03  W-IDPGM               PIC X(8)    VALUE SPACE.                     
005300                                                                          
005400 01  W-VIMSID.                                                            
005500   03  W-IMSID               PIC X(4)    VALUE SPACE.                     
005600   03                        PIC X(4)    VALUE SPACE.                     
005700                                                                          
005800 01  TEST-PRINTER-NAME       PIC X(8).                                    
005810* V6+VT PRINTRAR UTGÅTT. LÄMNAR KVAR VX FÖR EV FRAMTIDA BEHOV.            
005900 88  VX-PRINTER              VALUE 'QSE00000'.                            
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400   03  W006PRT               PIC X(8)    VALUE 'W006PRT '.                
006500   03  VIMSID                PIC X(8)    VALUE 'VIMSID  '.                
006600   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006700   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
006800                                                                          
006900     EJECT                                                                
007000 01  FILLER                  PIC X(16)   VALUE 'W006PRT '.                
007100*01  -COPY W006PRT.                                                       
007200                                                                          
007300     EJECT                                                                
007400 01  FILLER                  PIC X(16)   VALUE 'W006PRTY'.                
007500*01  -COPY W006PRTY.                                                      
007600                                                                          
007700     EJECT                                                                
007800 01  FILLER                  PIC X(16)   VALUE 'W006PRAR'.                
007900*01  -COPY W006PRAR.                                                      
008000                                                                          
008100     EJECT                                                                
008200 01  FILLER                  PIC X(16)   VALUE 'SPOOL-AREA'.              
008300*01  -COPY WMSGSPOL                                                       
008400                                                                          
008500     EJECT                                                                
008600******************************************************************        
008700*                                                                         
008800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008900*                                                                         
009000 01  IMS-WS.                                                              
009100   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
009200     SKIP3                                                                
009300*                        **** STATUS-KOD FRÅN IMS                         
009400   03  STATUS-WS             PIC XX.                                      
009500     88  SEGMENT-FINNS                   VALUE '  '.                      
009600                                                                          
009700   03  GODK-STATUSKODER.                                                  
009800     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  NYCKLAR-TILL-DLI.                                                    
010100   03  W-WDG801KY-X.                                                      
010200     05  W-IDLTERM           PIC X(8).                                    
010300     05  W-TIREGDAT          PIC S9(7)   VALUE ZERO  COMP-3.              
010400     05  W-TIKLOCK-9KOMPL    PIC S9(9)   VALUE ZERO  COMP-3.              
010500     SKIP3                                                                
010600 01  SSA1                    PIC X(64).                                   
010700 01  SSA2                    PIC X(64).                                   
010800     EJECT                                                                
010900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDG801'.           
011000                                                                          
011100 01  DLI-IO-WDG801.                                                       
011200*  03  -COPY WDG801  -PRE WDG8-.                                          
011300     EJECT                                                                
011400 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDG811'.           
011500                                                                          
011600 01  DLI-IO-WDG811.                                                       
011700*  03  -COPY WDG811  -PRE WDG8-.                                          
011800     EJECT                                                                
011900*                            IMS FUNKTIONSKODER                           
012000*01    -COPY W0003                                                        
012100                                                                          
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
012500 01  -COPY W006PRTY -PRE LINK-                                            
012600     EJECT                                                                
012700 01  LINK-CALL-TYP           PIC X(5).                                    
012800     SKIP2                                                                
012900 01  LINK-IDPRTLST           PIC X(8).                                    
013000     SKIP2                                                                
013100*01  -COPY W0009 -PRE ALT-.                                               
013200     EJECT                                                                
013300*01  -COPY W0008 -PRE WDG8-                                               
013400     05  FILLER              PIC X.                                       
013500     SKIP2                                                                
013600 01  LINK-IDENT.                                                          
013700   03  LINK-IDLIST           PIC X(10).                                   
013800   03  FILLER                PIC X(18).                                   
013900     SKIP2                                                                
014000 01  LINK-SKIP               PIC S9(3) COMP-3.                            
014100     SKIP2                                                                
014200 01  LINK-RAD                PIC X(132).                                  
014300     EJECT                                                                
014400 PROCEDURE DIVISION USING  LINK-PRTY-W006PRTY                             
014500                           LINK-CALL-TYP                                  
014600                           LINK-IDPRTLST                                  
014700                           ALT-PCB                                        
014800                           WDG8-PCB                                       
014900                           LINK-IDENT                                     
015000                           LINK-SKIP                                      
015100                           LINK-RAD.                                      
015200 STYR SECTION.                                                            
015300                                                                          
015400     EVALUATE LINK-CALL-TYP                                               
015500       WHEN PRT-OPEN  MOVE HIGH-VALUE TO W-IDPRTLST                       
015600                                         W-IDPRTLST-SPAR                  
015700                                         W-IDLIST                         
015800                                                                          
015900       WHEN PRT-WRITE PERFORM B-WRITE                                     
016000                                                                          
016100       WHEN PRT-PURGE PERFORM S03-SKRIV                                   
016200                      IF W-FAX-SW = JA                                    
016300                        PERFORM IMS-INSERT-SPOOL-FAX                      
016400                        MOVE NEJ TO W-FAX-SW                              
016500                      END-IF                                              
016600                      MOVE JA TO W-CHANGE-SW                              
016700                                                                          
016800       WHEN PRT-CLOSE PERFORM S03-SKRIV                                   
016900                      IF W-FAX-SW = JA                                    
017000                        PERFORM IMS-INSERT-SPOOL-FAX                      
017100                        MOVE NEJ TO W-FAX-SW                              
017200                      END-IF                                              
017300                      MOVE JA TO W-CHANGE-SW                              
017400     END-EVALUATE                                                         
017500                                                                          
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 B-WRITE SECTION.                                                         
018100*                         LINK-SKIP > 900 = NYSIDA                        
018200     MOVE LINK-SKIP TO W-SKIP                                             
018300     IF LINK-SKIP > W-NYSIDA                                              
018400       PERFORM S03-SKRIV                                                  
018500       IF LINK-IDLIST NOT = W-IDLIST                                      
018600         MOVE LINK-IDENT TO W-IDENT                                       
018700         MOVE JA TO W-PRT-NYLISTA                                         
018800       END-IF                                                             
018900       IF     LINK-IDPRTLST      NOT = W-IDPRTLST-SPAR                    
019000           OR LINK-PRTY-KDCOPIES NOT = PRTY-KDCOPIES                      
019100           OR LINK-PRTY-IDTFX    NOT = PRTY-IDTFX                         
019200         IF W-FAX-SW = JA                                                 
019300           PERFORM IMS-INSERT-SPOOL-FAX                                   
019400           MOVE NEJ TO W-FAX-SW                                           
019500         END-IF                                                           
019600         PERFORM BA-NYPRINTER                                             
019700         MOVE JA TO W-CHANGE-SW                                           
019800         MOVE JA TO W-PRT-NYLISTA                                         
019900       END-IF                                                             
020000       PERFORM S01-TAB-INIT                                               
020100     ELSE                                                                 
020200       IF LINK-SKIP > +800                                                
020300         MOVE W-SKIP TO W-IX                                              
020400       ELSE                                                               
020500         ADD LINK-SKIP TO W-IX                                            
020600       END-IF                                                             
020700       IF W-IX > W-MAX-IX                                                 
020800         MOVE W-MAX-IX TO W-IX                                            
020900         PERFORM S03-SKRIV                                                
021000         PERFORM S01-TAB-INIT                                             
021100       END-IF                                                             
021200     END-IF                                                               
021300     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
021400       MOVE LINK-RAD TO SPOOL-A4S-DATA (W-IX)                             
021500     ELSE                                                                 
021600       MOVE LINK-RAD TO SPOOL-RAD-DATA (W-IX)                             
021700     END-IF                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 BA-NYPRINTER SECTION.                                                    
022100                                                                          
022200     CALL VIMSID USING W-VIMSID                                           
022300     MOVE +900 TO W-NYSIDA                                                
022400     IF LINK-IDPRTLST = 'W475FAX '                                        
022500        AND (LINK-PRTY-IDPFDEF = 'W475FA  ' OR SPACE)                     
022600       MOVE 'FAXNOP  '    TO PRT-IDPRTLST                                 
022700                             W-IDPRTLST                                   
022800     ELSE                                                                 
022900       MOVE LINK-IDPRTLST TO PRT-IDPRTLST                                 
023000                             W-IDPRTLST                                   
023100     END-IF                                                               
023200     MOVE LINK-IDPRTLST TO W-IDPRTLST-SPAR                                
023300     MOVE '003' TO PRT-KDCALL                                             
023400     CALL W006PRT USING PRT-W006PRT                                       
023500     MOVE PRT-BEPRTLST TO W-PRTTYP                                        
023600     MOVE LINK-PRTY-W006PRTY TO PRTY-W006PRTY                             
023700     IF PRTY-IDPFDEF = 'W475FA  '                                         
023800       MOVE SPACE TO PRTY-IDPFDEF                                         
023900     END-IF                                                               
024000     IF PRTY-KDCOPIES NUMERIC                                             
024100       MOVE PRTY-KDCOPIES TO SPOOL-COPIES                                 
024200     ELSE                                                                 
024300       MOVE '1'      TO SPOOL-COPIES                                      
024400     END-IF                                                               
024500     EVALUATE PRTY-KDFORMS                                                
024600       WHEN 'B'       MOVE 'F001' TO SPOOL-FORMS                          
024700       WHEN 'C'       MOVE '4811' TO SPOOL-FORMS                          
024800       WHEN 'D'       MOVE 'VCAS' TO SPOOL-FORMS                          
024900       WHEN '2'       MOVE '2000' TO SPOOL-FORMS                          
025000       WHEN '3'       MOVE '3000' TO SPOOL-FORMS                          
025100       WHEN OTHER     MOVE 'STD ' TO SPOOL-FORMS                          
025200     END-EVALUATE                                                         
025300     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
025400        MOVE +85 TO W-RADL                                                
025500        MOVE +80 TO W-MAX-IX                                              
025600     ELSE                                                                 
025700       MOVE +137 TO W-RADL                                                
025800       MOVE +70 TO W-MAX-IX                                               
025900     END-IF                                                               
026000     IF W-PRTTYP = 'LASER' OR 'TERMO' OR 'ZEBRA'                          
026100       MOVE 'IAFP=N1M,PRTO=' TO SPOOL-IAFP                                
026200     ELSE                                                                 
026300       MOVE 'IAFP=A1M,PRTO=' TO SPOOL-IAFP                                
026400     END-IF                                                               
026500     MOVE PRT-IDNODE TO SPOOL-IDNODE                                      
026600     IF W-PRTTYP (1:3) = 'IBM'                                            
026700       IF PRTY-IDPFDEF = SPACE AND W-IDPRTLST (1:1) = 'W'                 
026800         MOVE W-IDPRTLST (1:6) TO PRTY-IDPFDEF                            
026900       END-IF                                                             
027000     END-IF                                                               
027100     IF PRTY-IDPFDEF NOT = SPACE                                          
027200       MOVE PRTY-IDPFDEF TO SPOOL-FORMDEF SPOOL-PAGEDEF                   
027300       MOVE 'A'  TO SPOOL-CLASS                                           
027400*            -- SET TO CLASS E FOR OLD JES2 PRINTERS                      
027500       IF PRT-IDNODE (1:4) = 'NJO0'                                       
027600         MOVE 'E'  TO SPOOL-CLASS                                         
027700       END-IF                                                             
027800                                                                          
027900       MOVE PRT-IDNODE  TO TEST-PRINTER-NAME                              
028000       IF W-IMSID = 'IMG0'                                                
028100         IF VX-PRINTER                                                    
028200* FÖR EV FRAMTIDA BEHOV                                                   
028202           MOVE 'W??.????.PSF)' TO SPOOL-USERLIB                          
028300           MOVE 13 TO SPOOL-USERLIB-LENGTH                                
028400         ELSE                                                             
028900           MOVE 'W.QASE.PSF)' TO SPOOL-USERLIB                            
029000           MOVE 11 TO SPOOL-USERLIB-LENGTH                                
029100         END-IF                                                           
029300       ELSE                                                               
030300         MOVE 'W.IGRT.PSF,W.PROD.PSF)' TO SPOOL-USERLIB                   
030400         MOVE 22 TO SPOOL-USERLIB-LENGTH                                  
030700       END-IF                                                             
030800       MOVE SPOOL-IBM-LASER TO SPOOL-OVR-PARAM                            
030900       COMPUTE SPOOL-IAFP-LL = LENGTH OF SPOOL-NORMAL-OPTIONS             
031000                             + LENGTH OF SPOOL-IBM-LASER-DEL1             
031100                             + SPOOL-USERLIB-LENGTH                       
031200     ELSE                                                                 
031300       COMPUTE SPOOL-IAFP-LL = LENGTH OF SPOOL-NORMAL-OPTIONS             
031400       MOVE 'A'  TO SPOOL-CLASS                                           
031500       IF PRT-IDNODE (1:3) = 'R31' OR 'N38'                               
031600         MOVE 'F001' TO SPOOL-FORMS                                       
031700       ELSE                                                               
031800         IF PRT-IDNODE = 'R3200371'                                       
031900           MOVE '4811' TO SPOOL-FORMS                                     
032000         END-IF                                                           
032100       END-IF                                                             
032200     END-IF                                                               
032300                                                                          
032400     IF W-IMSID = 'IM1P' OR 'IMG0'                                        
032500       CONTINUE                                                           
032600     ELSE                                                                 
032700       IF PRT-IDNODE = 'QSE09012' OR 'QSE09013'                           
032800                    OR 'NJOV1   ' OR 'NJOV2   '                           
032900         MOVE 'IAFP=A00,PRTO=' TO SPOOL-IAFP                              
033000         MOVE 'H' TO SPOOL-CLASS                                          
033100         MOVE ',OUTDISP(HOLD,HOLD)' TO SPOOL-OVR-PARAM1                   
033200         MOVE SPOOL-IBM-LASER       TO SPOOL-OVR-PARAM2                   
033300         COMPUTE SPOOL-IAFP-LL = SPOOL-IAFP-LL + 19                       
033400       END-IF                                                             
033500     END-IF                                                               
033600                                                                          
033700     COMPUTE SPOOL-OPT-LL  = SPOOL-IAFP-LL                                
033800                           + LENGTH OF SPOOL-START-OPTIONS                
033900     .                                                                    
034000     EJECT                                                                
034100 S01-TAB-INIT SECTION.                                                    
034200                                                                          
034300     MOVE +1 TO INDX                                                      
034400     IF PRTY-IDPRTSPO = 'SPO-A4S '                                        
034500       PERFORM 80 TIMES                                                   
034600         MOVE +85   TO SPOOL-A4S-RDW (INDX)                               
034700         MOVE ZERO  TO SPOOL-A4S-ZZ  (INDX)                               
034800         MOVE SPACE TO SPOOL-A4S     (INDX)                               
034900         ADD +1 TO INDX                                                   
035000       END-PERFORM                                                        
035100       MOVE '1' TO SPOOL-A4S-STYR (1)                                     
035200     ELSE                                                                 
035300       PERFORM 70 TIMES                                                   
035400         MOVE +137  TO SPOOL-RAD-RDW (INDX)                               
035500         MOVE ZERO  TO SPOOL-RAD-ZZ  (INDX)                               
035600         MOVE SPACE TO SPOOL-RAD     (INDX)                               
035700         ADD +1 TO INDX                                                   
035800       END-PERFORM                                                        
035900       MOVE '1' TO SPOOL-RAD-STYR (1)                                     
036000     END-IF                                                               
036100                                                                          
036200     IF W-SKIP > W-MAX-IX                                                 
036300       MOVE +1 TO W-IX                                                    
036400     ELSE                                                                 
036500       MOVE W-SKIP TO W-IX                                                
036600       IF W-PRTTYP = 'IBMLB' OR 'IBMLD'                                   
036700       AND  (PRTY-IDPFDEF(1:4) = 'W475')                                  
036800         ADD +1 TO W-IX                                                   
036900       END-IF                                                             
037000     END-IF                                                               
037100     MOVE JA TO W-SKRIV-SW                                                
037200     .                                                                    
037300     EJECT                                                                
037400 S03-SKRIV SECTION.                                                       
037500                                                                          
037600     IF W-SKRIV-SW = JA                                                   
037700       IF W-PRT-NYLISTA = JA                                              
037800         MOVE PRT-IDNODE TO WDG8-LIST-IDLTERM W-IDLTERM                   
037900         ACCEPT W-DATUM FROM DATE                                         
038000         MOVE W-DATUM TO WDG8-LIST-TIREGDAT W-TIREGDAT                    
038100         ACCEPT W-TIME FROM TIME                                          
038200         COMPUTE W-TIKLOCK-9KOMPL = W-NIOR - W-TIME                       
038300         MOVE W-IDLIST TO WDG8-LIST-IDLIST                                
038400         MOVE W-IDPRTLST TO WDG8-LIST-IDPRTLST                            
038500         MOVE NEJ TO WDG8-LIST-FLSKRIV                                    
038600         IF W-PRTTYP = 'NOPRT'                                            
038700             OR W-PRTTYP = 'IBMLC'                                        
038800             OR W-PRTTYP = 'IBMLD'                                        
038900           MOVE +0 TO WDG8-LIST-KVANTEX-PRINTAD                           
039000         ELSE                                                             
039100           MOVE +1 TO WDG8-LIST-KVANTEX-PRINTAD                           
039200         END-IF                                                           
039300         MOVE PRTY-W006PRTY TO WDG8-LIST-W006PRTY                         
039400         MOVE W-PRTTYP      TO WDG8-LIST-IDPRTTYP                         
039500         MOVE W-IDPGM       TO WDG8-LIST-IDPGM                            
039600         MOVE W-RADL        TO WDG8-LIST-KVLL                             
039700         MOVE +0 TO W-IDSID                                               
039800         MOVE HIGH-VALUE TO STATUS-WS                                     
039900         PERFORM UNTIL STATUS-WS = SPACE                                  
040000           SUBTRACT +1 FROM W-TIKLOCK-9KOMPL                              
040100           MOVE W-TIKLOCK-9KOMPL TO WDG8-LIST-TIKLOCK-9KOMPL              
040200           PERFORM IMS-ISRT-WDG8-ROT                                      
040300         END-PERFORM                                                      
040400         MOVE NEJ TO W-PRT-NYLISTA                                        
040500       END-IF                                                             
040600       ADD +1 TO W-IDSID                                                  
040700       MOVE W-IDSID TO WDG8-SID-IDSID                                     
040800       COMPUTE SPOOL-RAD-BDW = W-RADL * W-IX + 4                          
040900       PERFORM S03-SAVE-PAGE                                              
041000       PERFORM IMS-ISRT-WDG8-SIDAN                                        
041100       IF PRT-IDNODE = 'QSE09012' OR 'QSE09013'                           
041200         CONTINUE                                                         
041300       ELSE                                                               
041400         IF W-PRTTYP = 'LASER' OR 'TERMO' OR 'ZEBRA'                      
041500*                    STYRTECKEN TAS BORT FRÅN POS 1 I ALLA RADER          
041600           MOVE +1 TO INDX                                                
041700           IF PRTY-IDPRTSPO = 'SPO-A4S '                                  
041800             PERFORM 80 TIMES                                             
041900               MOVE SPOOL-A4S-DATA (INDX) TO SPOOL-A4S (INDX)             
042000               ADD +1 TO INDX                                             
042100             END-PERFORM                                                  
042200           ELSE                                                           
042300             PERFORM 70 TIMES                                             
042400               MOVE SPOOL-RAD-DATA (INDX) TO SPOOL-RAD (INDX)             
042500               ADD +1 TO INDX                                             
042600             END-PERFORM                                                  
042700           END-IF                                                         
042800         END-IF                                                           
042900       END-IF                                                             
043000       INSPECT SPOOL-RAD-AREA REPLACING ALL '*COPY*' BY SPACE             
043100       IF W-PRTTYP = 'NOPRT'                                              
043200           OR W-PRTTYP = 'IBMLC'                                          
043300           OR W-PRTTYP = 'IBMLD'                                          
043400         CONTINUE                                                         
043500       ELSE                                                               
043600         IF W-CHANGE-SW = JA                                              
043700           IF PRTY-IDTFX NOT = SPACE                                      
043800               AND PRT-IDNODE (1:3) = 'FAX'                               
043900             MOVE 'QSERFAX ' TO SPOOL-IDNODE                              
044000             MOVE 'A'  TO SPOOL-CLASS                                     
044100             MOVE '1'  TO SPOOL-COPIES                                    
044200             MOVE +65  TO SPOOL-FAX-RDW  (1)                              
044300             MOVE ZERO TO SPOOL-FAX-ZZ   (1)                              
044400             MOVE '1'  TO SPOOL-FAX-STYR (1)                              
044500             EVALUATE W-IMSID                                             
044600               WHEN 'IM1P'                                                
044700               WHEN 'IMG0'                                                
044800                 MOVE SPACE TO SPOOL-FAX-DATA (1)                         
044900                 STRING '*AUTOFAX*<TOFAXNUM:' PRTY-IDTFX                  
045000                     DELIMITED BY SIZE INTO SPOOL-FAX-DATA (1)            
045100                 INSPECT SPOOL-FAX-DATA (1)                               
045200                     REPLACING FIRST SPACE BY '>'                         
045300               WHEN OTHER                                                 
045400                 MOVE '*AUTOFAX* <TOFAXNUM:62612>'                        
045500                     TO SPOOL-FAX-DATA (1)                                
045600             END-EVALUATE                                                 
045700             MOVE +65  TO SPOOL-FAX-RDW  (2)                              
045800             MOVE ZERO TO SPOOL-FAX-ZZ   (2)                              
045900             MOVE ' '  TO SPOOL-FAX-STYR (2)                              
046000             MOVE '<FROMNAME:PULS FAX DISTRIBUTION SYSTEM>'               
046100                  TO SPOOL-FAX-DATA (2)                                   
046200             MOVE +65  TO SPOOL-FAX-RDW  (3)                              
046300             MOVE ZERO TO SPOOL-FAX-ZZ   (3)                              
046400             MOVE ' '  TO SPOOL-FAX-STYR (3)                              
046500             MOVE '<TONAME:FAX RECEIVER>'                                 
046600                  TO SPOOL-FAX-DATA (3)                                   
046700             MOVE +1 TO INDX                                              
046800             PERFORM 5 TIMES                                              
046900               MOVE +65  TO SPOOL-FAX-RDW  (INDX + 3)                     
047000               MOVE ZERO TO SPOOL-FAX-ZZ   (INDX + 3)                     
047100               MOVE ' '  TO SPOOL-FAX-STYR (INDX + 3)                     
047200               STRING '<NOTE:' PRTY-TEFAX (INDX) '>   '                   
047300                 DELIMITED BY SIZE INTO SPOOL-FAX-DATA (INDX + 3)         
047400               ADD +1 TO INDX                                             
047500             END-PERFORM                                                  
047600             MOVE +65  TO SPOOL-FAX-RDW  (9)                              
047700             MOVE ZERO TO SPOOL-FAX-ZZ   (9)                              
047800             MOVE ' '  TO SPOOL-FAX-STYR (9)                              
047900             MOVE '<DELETELASTPAGE>'                                      
048000                  TO SPOOL-FAX-DATA (9)                                   
048100             MOVE JA TO W-FAX-SW                                          
048200           END-IF                                                         
048300           PERFORM IMS-CHANGE-SPOOL                                       
048400         END-IF                                                           
048500         PERFORM IMS-INSERT-SPOOL                                         
048600       END-IF                                                             
048700       MOVE NEJ TO W-SKRIV-SW                                             
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 S03-SAVE-PAGE SECTION.                                                   
049200                                                                          
049300*                      TAR BORT TOMMA RADER OCH GÖR OM SIDAN              
049400*                      TILL ETT VB-SEGMENT                                
049500                                                                          
049600     MOVE SPOOL-RAD-BDW TO WDG8-SID-KVLL-BDW                              
049700     MOVE 01    TO WDG8-SID-IDPRTRAD (1)                                  
049800     MOVE SPACE TO WDG8-SID-TEPRTRAD (1)                                  
049900     MOVE +1 TO INDX                                                      
050000     MOVE +0 TO INDX2                                                     
050100     IF PRTY-IDPRTSPO = 'SPO-A4S'                                         
050200       MOVE SPOOL-A4S-RDW (1) TO WDG8-SID-KVLL-RDW                        
050300       PERFORM 80 TIMES                                                   
050400         IF SPOOL-A4S-DATA (INDX) > SPACE                                 
050500           ADD +1 TO INDX2                                                
050600           MOVE SPOOL-A4S-DATA (INDX)                                     
050700                TO WDG8-SID-TEPRTRAD (INDX2)                              
050800           MOVE INDX TO WDG8-SID-IDPRTRAD (INDX2)                         
050900         END-IF                                                           
051000         ADD +1 TO INDX                                                   
051100       END-PERFORM                                                        
051200     ELSE                                                                 
051300       MOVE SPOOL-RAD-RDW (1) TO WDG8-SID-KVLL-RDW                        
051400       PERFORM 70 TIMES                                                   
051500         IF SPOOL-RAD-DATA (INDX) > SPACE                                 
051600           ADD +1 TO INDX2                                                
051700           MOVE SPOOL-RAD-DATA (INDX)                                     
051800                TO WDG8-SID-TEPRTRAD (INDX2)                              
051900           MOVE INDX TO WDG8-SID-IDPRTRAD (INDX2)                         
052000         END-IF                                                           
052100         ADD +1 TO INDX                                                   
052200       END-PERFORM                                                        
052300     END-IF                                                               
052400     IF INDX2 = ZERO                                                      
052500       MOVE +1 TO INDX2                                                   
052600     END-IF                                                               
052700     COMPUTE WDG8-SID-KVLL = INDX2 * 134 + 8                              
052800     .                                                                    
052900     EJECT                                                                
053000* IMS SEKTIONER                                                           
053100                                                                          
053200 IMS-CHANGE-SPOOL SECTION.                                                
053300                                                                          
053400     IF W-PRTTYP = 'NOPRT'                                                
053500         OR W-PRTTYP = 'IBMLC'                                            
053600         OR W-PRTTYP = 'IBMLD'                                            
053700       CONTINUE                                                           
053800     ELSE                                                                 
053900       MOVE SPACE TO GODK-STATUSKODER                                     
054000       CALL CBLTDLI USING CHNG ALT-PCB SPOOL-OPT-DEST                     
054100                               SPOOL-OPTIONS SPOOL-FEEDBACK               
054200       MOVE ALT-STATUS-CODE TO STATUS-WS                                  
054300       PERFORM IMS-STATUSKONTROLL-SPOOL                                   
054400       MOVE NEJ TO W-CHANGE-SW                                            
054500     END-IF                                                               
054600     .                                                                    
054700                                                                          
054800                                                                          
054900 IMS-INSERT-SPOOL SECTION.                                                
055000                                                                          
055100     MOVE SPACE TO GODK-STATUSKODER                                       
055200     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-RAD-AREA                       
055300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
055400     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
055500     .                                                                    
055600                                                                          
055700                                                                          
055800 IMS-INSERT-SPOOL-FAX SECTION.                                            
055900                                                                          
056000     MOVE SPACE TO GODK-STATUSKODER                                       
056100     CALL CBLTDLI USING ISRT ALT-PCB SPOOL-FAX-AREA                       
056200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
056300     PERFORM IMS-STATUSKONTROLL-SPOOL                                     
056400     .                                                                    
056500                                                                          
056600                                                                          
056700     EJECT                                                                
056800                                                                          
056900 IMS-ISRT-WDG8-ROT SECTION.                                               
057000     IF WDG8-DBD-NAME = 'WDG8'                                            
057100       MOVE 'WDG801   ' TO SSA1                                           
057200     ELSE                                                                 
057300       MOVE 'WLLISB01 ' TO SSA1                                           
057400     END-IF                                                               
057500     MOVE '  II' TO GODK-STATUSKODER                                      
057600     CALL CBLTDLI USING ISRT WDG8-PCB DLI-IO-WDG801 SSA1                  
057700     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
057800     PERFORM IMS-STATUSKONTROLL                                           
057900     .                                                                    
058000                                                                          
058100                                                                          
058200 IMS-ISRT-WDG8-SIDAN SECTION.                                             
058300     IF WDG8-DBD-NAME = 'WDG8'                                            
058400       STRING 'WDG801  (WDG801KY =' W-WDG801KY-X ')'                      
058500              DELIMITED BY SIZE INTO SSA1                                 
058600       MOVE 'WDG811   ' TO SSA2                                           
058700     ELSE                                                                 
058800       STRING 'WLLISB01(WDG801KY =' W-WDG801KY-X ')'                      
058900              DELIMITED BY SIZE INTO SSA1                                 
059000       MOVE 'WLLISB11 ' TO SSA2                                           
059100     END-IF                                                               
059200     MOVE '  II' TO GODK-STATUSKODER                                      
059300     CALL CBLTDLI USING ISRT WDG8-PCB DLI-IO-WDG811 SSA1 SSA2             
059400     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
059500     PERFORM IMS-STATUSKONTROLL                                           
059600     .                                                                    
059700     EJECT                                                                
059800 IMS-STATUSKONTROLL-SPOOL SECTION.                                        
059900                                                                          
060000     SET STATUS-IX TO 1                                                   
060100     SEARCH GODK-STATUS                                                   
060200       AT END                                                             
060300         MOVE 'FEL FRÅN IMS SPOOL-INTERFACE SE SPOOL-FEEDBACK'            
060400           TO FELTEXT                                                     
060500         CALL FELLOG                                                      
060600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
060700         CONTINUE                                                         
060800     END-SEARCH                                                           
060900     .                                                                    
061000                                                                          
061100                                                                          
061200 IMS-STATUSKONTROLL SECTION.                                              
061300                                                                          
061400     SET STATUS-IX TO 1                                                   
061500     SEARCH GODK-STATUS                                                   
061600       AT END                                                             
061700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
061800         DELIMITED BY SIZE INTO FELTEXT                                   
061900         CALL FELLOG                                                      
062000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
062100         CONTINUE                                                         
062200     END-SEARCH                                                           
063000     .                                                                    
