000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     WB112000.                                                
000500 AUTHOR.         CONNY EGHOLT.                                            
000600 DATE-WRITTEN.   DECEMBER 2003.                                           
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*   PROGRAMMET LÄSER, GÖR UPDATE & INSERT                                 
001000*                       I TABELLEN TB1ACCE (TILLBEHÖR)                    
001100*                                                                         
001200*   ÄNDRINGAR:                                                            
001300*        2005-03-17  NYTT DATAELEMENT "FLANNULL" SOM BESKRIVER            
001400*        ATT ARTIKELN INTE LÄNGRE KOMMER FRÅN KDP.                        
001500*        FLANNULL BESTÄMMS I WB112200 GENOM MATCHNING AV +0 & -1          
001600*                                                                         
001700*        OM ART. ÅTERKOMMER I KDP, SÄTTS FLANNULL TILL 'N' IGEN.          
001800*       /CE                                                               
001900*                                                                         
002000*        2007-03-23  Ny nyckel för TB1ACCE.  IDARTNR + IDUPPDSU           
002010*                    Nya och borttagna fält i TB1ACCE                     
002020*                    Nya fält i PC0234F2 f.v.b. WB1122                    
002100*                                                                         
002110*    2015-1      ETRACKER 10219962                                        
002120*                                                                         
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700 ENVIRONMENT DIVISION.                                                    
002800 INPUT-OUTPUT SECTION.                                                    
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*       ---- UPPDATERINGSPOSTER FRÅN KDP VIA VCOM ----                    
003200     SELECT WB1122                     ASSIGN TO WB1120D1.                
003300                                                                          
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 FILE SECTION.                                                            
003700                                                                          
003800 FD  WB1122                                                               
003900     RECORDING F                                                          
004000     BLOCK CONTAINS  0.                                                   
004100 01  KDP-POST .                                                           
004200*    03 -COPY PC0234F2    -L.                                             
004300     03 KDANNULL       PIC X.                                             
004400                                                                          
004500*                                                                         
004600     EJECT                                                                
004700*                                                                         
004800 WORKING-STORAGE SECTION.                                                 
004900*                                                                         
005000 77  IDPGM                     PIC X(8)  VALUE 'WB112000'.                
005100                                                                          
005200 77  NEJ                       PIC X(1)  VALUE 'N'.                       
005300 77  JA                        PIC X(1)  VALUE 'J'.                       
005400 77  ANNULL                    PIC X(1)  VALUE 'A'.                       
005500                                                                          
005600                                                                          
005700 77  WB1122-EOF-SW             PIC X       VALUE 'N'.                     
005800     88  END-OF-WB1122                     VALUE 'J'.                     
005900                                                                          
006000                                                                          
006100 01  FILLER                    PIC X(16) VALUE '80 FELTEXT ---->'.        
006200 01  FELTEXT                   PIC X(80) VALUE SPACE.                     
006300                                                                          
006400 01  FILLER                    PIC X(16) VALUE 'DB2-SECTION --->'.        
006500 01  DB2-SECTION-TEXT          PIC X(32) VALUE SPACE.                     
006600                                                                          
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800     03  ABEND                 PIC X(8)    VALUE 'ABEND   '.              
006900     03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.              
007000     03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.              
007100     03  W009VADD              PIC X(8)    VALUE 'W009VADD'.              
007200     EJECT                                                                
007300                                                                          
007400 01  RKOD-ABEND-DB2              PIC S9(4) VALUE +16   COMP SYNC.         
007500 01  RKOD-ABEND-MED-DUMP         PIC S9(4) VALUE +1000 COMP SYNC.         
007600     EJECT                                                                
007700                                                                          
007800 01  FILLER                      PIC X(16) VALUE 'WS-AREA'.               
007900 01 WS-AREA.                                                              
008000     03 DAGENS-DATUM             PIC 9(6)  VALUE ZERO.                    
008100     03 FILLER REDEFINES DAGENS-DATUM.                                    
008200         05 DAGENS-DATUM-AA      PIC 9(2).                                
008300         05 DAGENS-DATUM-MM      PIC 9(2).                                
008400         05 DAGENS-DATUM-DD      PIC 9(2).                                
008500                                                                          
008600     03  WS-ANTAL-INSERT         PIC 9(9)  VALUE ZERO.                    
008700     03  WS-ANTAL-UPDATE         PIC 9(9)  VALUE ZERO.                    
008800                                                                          
008900     EJECT                                                                
009000                                                                          
009100 01  FILLER                      PIC X(16) VALUE 'WDATAREA    '.          
009200*    ---PARAMETRAR TILL DATKONV                                           
009300*01  -COPY WDATAREA                                                       
009400     EJECT                                                                
009500                                                                          
009600 01  FILLER                      PIC X(16) VALUE 'POSTSUM'.               
009700*    ---PARAMETRAR TILL POSTSUM                                           
009800*01  -COPY W0005    -PRE W0005-                                           
009900     EJECT                                                                
010000                                                                          
010100                                                                          
010200 01  IN-AREA                     PIC X(24) VALUE 'IN-AREA-START'.         
010300     SKIP2                                                                
010400 01  FILLER                      PIC X(16)  VALUE 'WB1122-AREA'.          
010500 01  WB1122-AREA.                                                         
010600*    03 -COPY PC0234F2 -PRE WB1122- .                                     
010700     03 WB1122-KDANNULL       PIC X.                                      
010800                                                                          
010900     EJECT                                                                
011000*                                                                         
011100*                                                                         
011200*    ARBETSAREOR FÖR DB2-SEKTIONER                                        
011300*                                                                         
011400 01  FILLER           PIC X(16)  VALUE 'ACCE-AREA  '.                     
011500*01  AREA -COPY TB1ACCE    -PRE ACCE-                                     
011600     EJECT                                                                
011700 01  FILLER           PIC X(24)  VALUE '"DCL" DeCLaration table'.         
011800       EXEC SQL  INCLUDE TB1ACCE    END-EXEC.                             
011900     EJECT                                                                
012000 01  FILLER           PIC X(24)  VALUE 'SQL Communication Area'.          
012100       EXEC SQL  INCLUDE SQLCA      END-EXEC.                             
012200*                                                                         
012300*                                                                         
012400*                        **** NYCKLAR                                     
012500 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR'.              
012600 01  NYCKLAR.                                                             
012700     03 W-IDARTNR-X.                                                      
012800        05 W-IDARTNR            PIC S9(9) COMP-3.                         
012900     03 W-UPDNR-SU-X.                                                     
013000        05 W-IDUPPDSU           PIC X(8).                                 
013100*                                                                         
013200*                        **** STATUSKOD  FRÅN DB2                         
013300 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
013400 01  DB2-WS.                                                              
013500   03  SQLCODE-WS                PIC  9(3)  VALUE ZERO.                   
013600     88  CURSOR-OK                       VALUE  000.                      
013700     88  ROW-FOUND                       VALUE  000.                      
013800     88  ROW-NOT-FOUND                   VALUE  100.                      
013900     88  ROW-DUPLICATE                   VALUE  803.                      
014000     88  ROW-SEVERAL                     VALUE  811.                      
014100     88  RESOURCE-WRONG                  VALUE  904.                      
014200                                                                          
014300   03  GOOD-SQLCODESKODER.                                                
014400     05  GOOD-SQLCODE OCCURS 5                                            
014500         INDEXED BY SQLCODE-IX   PIC  9(3).                               
014600     EJECT                                                                
014700                                                                          
014800 PROCEDURE DIVISION.                                                      
014900 MAIN SECTION.                                                            
015000     ENTRY 'DLITCBL'.                                                     
015100                                                                          
015200     PERFORM A-INITIALIZE                                                 
015300                                                                          
015400     PERFORM S01-LAES-WB1122                                              
015500                                                                          
015600     PERFORM UNTIL END-OF-WB1122                                          
015700*      --- Bestäm behandlingstyp: Läs TB1ACCE                             
015800       MOVE WB1122-ARTNR     TO W-IDARTNR                                 
015900       MOVE WB1122-UPDNR-SU  TO W-IDUPPDSU                                
016000       PERFORM DB2-SELECT-ROW-TB1ACCE                                     
016100                                                                          
016200       IF ROW-FOUND                                                       
016300         PERFORM B-REPLACE-KDP-ELEMENTS                                   
016400         PERFORM DB2-REPLACE-ROW-TB1ACCE                                  
016500         ADD +1 TO WS-ANTAL-UPDATE                                        
016600       ELSE                                                               
016700         PERFORM C-INITIATE-ACCE-ELEMENTS                                 
016800         PERFORM DB2-INSERT-ROW-TB1ACCE                                   
016900         ADD +1 TO WS-ANTAL-INSERT                                        
017000       END-IF                                                             
017100                                                                          
017200       PERFORM S01-LAES-WB1122                                            
017300     END-PERFORM                                                          
017400                                                                          
017500     PERFORM Z-FINITO                                                     
017600                                                                          
017700     MOVE ZERO TO RETURN-CODE                                             
017800     GOBACK                                                               
017900     .                                                                    
018000     EJECT                                                                
018100                                                                          
018200 A-INITIALIZE SECTION.                                                    
018300     SKIP2                                                                
018400     INITIALIZE GOOD-SQLCODESKODER                                        
018500                                                                          
018600     OPEN INPUT WB1122                                                    
018700                                                                          
018800     ACCEPT DAGENS-DATUM  FROM DATE                                       
018900                                                                          
019000     MOVE 'WB1120'   TO W0005-PROGNAMN                                    
019100     .                                                                    
019200     EJECT                                                                
019300                                                                          
019400 B-REPLACE-KDP-ELEMENTS    SECTION.                                       
019500     SKIP2                                                                
019600*    ############################################################         
019700*    # Här ersätts de dataelement i TB1ACCE som kommer från KDP.#         
019800*    # Viss elementär kontroll för att ej sabba bef. data görs. #         
019900*    # Ev. tomma fält är alltid initierade till rätt dataklass. #         
020600*    ############################################################         
020610     MOVE WB1122-FKNGRUPPNR       TO ACCE-IDFKNGRP                        
020700     MOVE WB1122-ARTTYP (1:1)     TO ACCE-KDARTTYP                        
020701     IF WB1122-AONR-T NOT = SPACE                                         
020702       MOVE WB1122-AONR-T         TO ACCE-IDAOT                           
020703     END-IF                                                               
020704     IF WB1122-AOUTG-T NOT = SPACE                                        
020705       MOVE WB1122-AOUTG-T        TO ACCE-IDAOTUTG                        
020706     END-IF                                                               
020707     IF WB1122-AOINFTID6-T NOT = ZERO                                     
020708       MOVE WB1122-AOINFTID6-T    TO ACCE-TIAOINF                         
020709     END-IF                                                               
020710     MOVE WB1122-FARGSTATUS       TO ACCE-KDFARGST                        
020720     MOVE WB1122-ARTBEN-ENG       TO ACCE-BEART                           
020750     MOVE WB1122-GENANV-ENG       TO ACCE-TENOTE                          
021700     MOVE WB1122-UPDNR-KU         TO ACCE-IDUPPDKU                        
022000     IF WB1122-NAMN-ANSV-KU NOT = SPACE                                   
022100       MOVE WB1122-NAMN-ANSV-KU   TO ACCE-BEANST-KU                       
022200     END-IF                                                               
022300     IF WB1122-NAMN-ANSV-KU NOT = SPACE                                   
022310       MOVE WB1122-NAMN-ANSV-SU   TO ACCE-BEANST-SU                       
022320     END-IF                                                               
022321     MOVE WB1122-LONG-BENAMN-KU   TO ACCE-BEUPPDKU                        
022322     MOVE WB1122-LONG-BENAMN-SU   TO ACCE-BEUPPDSU                        
022330     MOVE WB1122-VIKT             TO ACCE-VKART-KDP                       
026800     MOVE WB1122-MDSMARK          TO ACCE-KDMDS                           
026810     MOVE WB1122-ARTUTFGILT       TO ACCE-TEARTUTFG                       
026820     MOVE WB1122-UPDNR-STATUS     TO ACCE-TESTATUPP                       
026900                                                                          
026901     MOVE WB1122-GSDBNR           TO ACCE-IDLEVNR-GSDB                    
026910     MOVE WB1122-TPD-ST-PH1       TO ACCE-KDTPD-PH1                       
026920     MOVE WB1122-TPD-WEEK-PH1     TO ACCE-DATPDPH1                        
026921                                                                          
026930     MOVE WB1122-QPSW-P-WK-PH1    TO ACCE-DAPSWQP-1                       
026940     MOVE WB1122-QPSW-P-ST-PH1    TO ACCE-KDPSWQP-1                       
026950     MOVE WB1122-QPSW-A-WK-PH1    TO ACCE-DAPSWQA-1                       
026951     MOVE WB1122-QPSW-A-ST-PH1    TO ACCE-KDPSWQA-1                       
026960                                                                          
026961     MOVE WB1122-PPSW-P-WK-PH2    TO ACCE-DAPSWPP-2                       
026962     MOVE WB1122-PPSW-P-ST-PH2    TO ACCE-KDPSWPP-2                       
026963     MOVE WB1122-PPSW-A-WK-PH2    TO ACCE-DAPSWPA-2                       
026964     MOVE WB1122-PPSW-A-ST-PH2    TO ACCE-KDPSWPA-2                       
026965                                                                          
026966     MOVE WB1122-CPSW-P-WK-PH3    TO ACCE-DAPSWCP-3                       
026967     MOVE WB1122-CPSW-P-ST-PH3    TO ACCE-KDPSWCP-3                       
026968     MOVE WB1122-CPSW-A-WK-PH3    TO ACCE-DAPSWCA-3                       
026969     MOVE WB1122-CPSW-A-ST-PH3    TO ACCE-KDPSWCA-3                       
026970                                                                          
026971     MOVE WB1122-POS              TO ACCE-IDKDPPOS                        
026972     MOVE WB1122-PSLAG            TO ACCE-IDPSLAG                         
026973     MOVE WB1122-TYPNR            TO ACCE-IDTYPNR                         
026974     MOVE WB1122-TYPBET           TO ACCE-BETYP                           
026975     MOVE WB1122-ARTNR-OFARGAT    TO ACCE-IDARTNR-OFARG                   
026976     MOVE WB1122-PROJEKT          TO ACCE-IDPROJK                         
026977     MOVE WB1122-PSSID            TO ACCE-IDPSS                           
026978     MOVE WB1122-KDANNULL         TO ACCE-KDANNULL                        
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 C-INITIATE-ACCE-ELEMENTS   SECTION.                                      
027500     SKIP2                                                                
027600     INITIALIZE ACCE-AREA                                                 
027610                                                                          
027700     MOVE WB1122-ARTNR        TO  ACCE-IDARTNR                            
027800                                     W-IDARTNR                            
027900     MOVE WB1122-FKNGRUPPNR   TO  ACCE-IDFKNGRP                           
027902     MOVE WB1122-ARTTYP(1:1)  TO  ACCE-KDARTTYP                           
027910     MOVE WB1122-AONR-T       TO  ACCE-IDAOT                              
027911     MOVE WB1122-AOUTG-T      TO  ACCE-IDAOTUTG                           
027920     MOVE WB1122-AOINFTID6-T  TO  ACCE-TIAOINF                            
027940     MOVE WB1122-FARGSTATUS   TO  ACCE-KDFARGST                           
028000     MOVE WB1122-ARTBEN-ENG   TO  ACCE-BEART                              
028010     MOVE WB1122-GENANV-ENG   TO  ACCE-TENOTE                             
028400     MOVE WB1122-UPDNR-KU     TO  ACCE-IDUPPDKU                           
028500     MOVE WB1122-UPDNR-SU     TO  ACCE-IDUPPDSU                           
028600                                     W-IDUPPDSU                           
028700     MOVE WB1122-LONG-BENAMN-KU                                           
028710                              TO  ACCE-BEUPPDKU                           
028720     MOVE WB1122-LONG-BENAMN-SU                                           
028730                              TO  ACCE-BEUPPDSU                           
028800     MOVE WB1122-NAMN-ANSV-KU TO  ACCE-BEANST-KU                          
028900     MOVE WB1122-NAMN-ANSV-SU TO  ACCE-BEANST-SU                          
029000     MOVE WB1122-VIKT         TO  ACCE-VKART-KDP                          
029200     MOVE WB1122-MDSMARK      TO  ACCE-KDMDS                              
029300     MOVE WB1122-ARTUTFGILT   TO  ACCE-TEARTUTFG                          
029410     MOVE WB1122-UPDNR-STATUS TO  ACCE-TESTATUPP                          
029411     MOVE WB1122-GSDBNR       TO  ACCE-IDLEVNR-GSDB                       
029412                                                                          
029413     MOVE WB1122-TPD-ST-PH1   TO  ACCE-KDTPD-PH1                          
029414     MOVE WB1122-TPD-WEEK-PH1 TO  ACCE-DATPDPH1                           
029415                                                                          
029416     MOVE WB1122-QPSW-P-WK-PH1 TO ACCE-DAPSWQP-1                          
029417     MOVE WB1122-QPSW-P-ST-PH1 TO ACCE-KDPSWQP-1                          
029418     MOVE WB1122-QPSW-A-WK-PH1 TO ACCE-DAPSWQA-1                          
029419     MOVE WB1122-QPSW-A-ST-PH1 TO ACCE-KDPSWQA-1                          
029420                                                                          
029421     MOVE WB1122-PPSW-P-WK-PH2 TO ACCE-DAPSWPP-2                          
029422     MOVE WB1122-PPSW-P-ST-PH2 TO ACCE-KDPSWPP-2                          
029423     MOVE WB1122-PPSW-A-WK-PH2 TO ACCE-DAPSWPA-2                          
029424     MOVE WB1122-PPSW-A-ST-PH2 TO ACCE-KDPSWPA-2                          
029425                                                                          
029426     MOVE WB1122-CPSW-P-WK-PH3 TO ACCE-DAPSWCP-3                          
029427     MOVE WB1122-CPSW-P-ST-PH3 TO ACCE-KDPSWCP-3                          
029428     MOVE WB1122-CPSW-A-WK-PH3 TO ACCE-DAPSWCA-3                          
029429     MOVE WB1122-CPSW-A-ST-PH3 TO ACCE-KDPSWCA-3                          
029441                                                                          
029445     MOVE WB1122-POS           TO ACCE-IDKDPPOS                           
029446     MOVE WB1122-PSLAG         TO ACCE-IDPSLAG                            
029447     MOVE WB1122-TYPNR         TO ACCE-IDTYPNR                            
029448     MOVE WB1122-TYPBET        TO ACCE-BETYP                              
029449     MOVE WB1122-ARTNR-OFARGAT TO ACCE-IDARTNR-OFARG                      
029450     MOVE WB1122-PROJEKT       TO ACCE-IDPROJK                            
029451     MOVE WB1122-PSSID         TO ACCE-IDPSS                              
029452     MOVE WB1122-KDANNULL      TO ACCE-KDANNULL                           
029460                                                                          
030000     .                                                                    
030100     EJECT                                                                
030200                                                                          
030300 S01-LAES-WB1122 SECTION.                                                 
030400     SKIP2                                                                
030500     READ WB1122 INTO WB1122-AREA                                         
030600       AT END                                                             
030700          SET  END-OF-WB1122 TO TRUE                                      
030800     END-READ                                                             
030900                                                                          
031000     IF END-OF-WB1122                                                     
031100       MOVE 'KDP'       TO W0005-TRANSTYP                                 
031200       MOVE 'WB1122'    TO W0005-FDNAMN                                   
031300       MOVE 'WB1120D1'  TO W0005-DDNAMN2                                  
031400       CALL POSTSUM  USING W0005-PARM                                     
031500     END-IF                                                               
031600     .                                                                    
031700     EJECT                                                                
031800*                                                                         
031900*S99-ABEND     SECTION.                                                   
032000*    SKIP2                                                                
032100*    DISPLAY FELTEXT                                                      
032200*    CALL ABEND USING RKOD-ABEND-DB2                                      
032300*    .                                                                    
032400     EJECT                                                                
032500 Z-FINITO      SECTION.                                                   
032600     SKIP2                                                                
032700     DISPLAY 'ANTAL INSERT : ' WS-ANTAL-INSERT                            
032800     DISPLAY 'ANTAL UPDATE : ' WS-ANTAL-UPDATE                            
032900                                                                          
033000     MOVE 'S' TO W0005-OPKOD                                              
033100     CALL POSTSUM USING W0005-PARM                                        
033200                                                                          
033300     CLOSE WB1122                                                         
033400     .                                                                    
033500     EJECT                                                                
033600*                                                                         
033700* ### DB2 SECTIONS  ########################################              
033800*                                                                         
033900                                                                          
034000 DB2-SELECT-ROW-TB1ACCE SECTION.                                          
034100     MOVE 'DB2-SELECT-ROW-TB1ACCE ' TO DB2-SECTION-TEXT                   
034200                                                                          
034300* SQL - Hämtar 2 Nycklar + 31 kolumner, som hänförs till KDP-data.        
034400     EXEC SQL  SELECT                                                     
034500                  IDARTNR,                                                
034600                  BEART,                                                  
034700                  IDUPPDSU,                                               
034710                  BEUPPDSU,                                               
034800                  BEANST_SU,                                              
034900                  IDUPPDKU,                                               
034910                  BEUPPDKU,                                               
035000                  BEANST_KU,                                              
035100                  IDFKNGRP,                                               
035400                  IDAOT,                                                  
035500                  IDAOTUTG,                                               
035600                  TIAOINF,                                                
035610                  KDARTTYP,                                               
035700                  VKART_KDP,                                              
035800                  KDMDS,                                                  
035900                  KDFARGST,                                               
035910                  TEARTUTFG,                                              
035920                  TESTATUPP,                                              
035930                  IDLEVNR_GSDB,                                           
035940                  KDTPD_PH1,                                              
035950                  DATPDPH1,                                               
035960                  DAPSWQP_1,                                              
035970                  KDPSWQP_1,                                              
035980                  DAPSWQA_1,                                              
035990                  KDPSWQA_1,                                              
035991                  DAPSWPP_2,                                              
035992                  KDPSWPP_2,                                              
035993                  DAPSWPA_2,                                              
035994                  KDPSWPA_2,                                              
035995                  DAPSWCP_3,                                              
035996                  KDPSWCP_3,                                              
035997                  DAPSWCA_3,                                              
035998                  KDPSWCA_3,                                              
036000                  TENOTE,                                                 
036100                  FLANNULL,                                               
036110                  IDKDPPOS,                                               
036120                  IDPSLAG,                                                
036130                  IDTYPNR,                                                
036140                  BETYP,                                                  
036150                  IDARTNR_OFARG,                                          
036200                  IDPROJK,                                                
036210                  IDPSS,                                                  
036220                  KDANNULL                                                
036230                                                                          
036300               INTO                                                       
036400                  :ACCE-IDARTNR,                                          
036500                  :ACCE-BEART,                                            
036700                  :ACCE-IDUPPDSU,                                         
036710                  :ACCE-BEUPPDSU,                                         
036800                  :ACCE-BEANST-SU,                                        
036900                  :ACCE-IDUPPDKU,                                         
036910                  :ACCE-BEUPPDKU,                                         
037000                  :ACCE-BEANST-KU,                                        
037100                  :ACCE-IDFKNGRP,                                         
037300                  :ACCE-IDAOT,                                            
037400                  :ACCE-IDAOTUTG,                                         
037500                  :ACCE-TIAOINF,                                          
037510                  :ACCE-KDARTTYP,                                         
037600                  :ACCE-VKART-KDP,                                        
037700                  :ACCE-KDMDS,                                            
037800                  :ACCE-KDFARGST,                                         
037810                  :ACCE-TEARTUTFG,                                        
037820                  :ACCE-TESTATUPP,                                        
037830                  :ACCE-IDLEVNR-GSDB,                                     
037840                  :ACCE-KDTPD-PH1,                                        
037850                  :ACCE-DATPDPH1,                                         
037860                  :ACCE-DAPSWQP-1,                                        
037870                  :ACCE-KDPSWQP-1,                                        
037880                  :ACCE-DAPSWQA-1,                                        
037890                  :ACCE-KDPSWQA-1,                                        
037891                  :ACCE-DAPSWPP-2,                                        
037892                  :ACCE-KDPSWPP-2,                                        
037893                  :ACCE-DAPSWPA-2,                                        
037894                  :ACCE-KDPSWPA-2,                                        
037895                  :ACCE-DAPSWCP-3,                                        
037896                  :ACCE-KDPSWCP-3,                                        
037897                  :ACCE-DAPSWCA-3,                                        
037898                  :ACCE-KDPSWCA-3,                                        
037900                  :ACCE-TENOTE,                                           
038000                  :ACCE-FLANNULL,                                         
038010                  :ACCE-IDKDPPOS,                                         
038020                  :ACCE-IDPSLAG,                                          
038030                  :ACCE-IDTYPNR,                                          
038040                  :ACCE-BETYP,                                            
038050                  :ACCE-IDARTNR-OFARG,                                    
038060                  :ACCE-IDPROJK,                                          
038070                  :ACCE-IDPSS,                                            
038080                  :ACCE-KDANNULL                                          
038100               FROM                                                       
038200                  TB1ACCE                                                 
038400               WHERE IDARTNR = :W-IDARTNR                                 
038410               AND   IDUPPDSU = :W-IDUPPDSU                               
038500     END-EXEC                                                             
038600                                                                          
038700* COBOL                                                                   
038800     MOVE 000100   TO GOOD-SQLCODESKODER                                  
038900     MOVE SQLCODE  TO SQLCODE-WS                                          
039000     PERFORM DB2-STATUSKONTROLL                                           
039100     .                                                                    
039200     EJECT                                                                
039300                                                                          
039400 DB2-REPLACE-ROW-TB1ACCE SECTION.                                         
039500     MOVE 'DB2-REPLACE-ROW-TB1ACCE ' TO DB2-SECTION-TEXT                  
039600* SQL - Skriver tilbaka alla 33 datafält efter uppdatering                
039700     EXEC SQL  UPDATE                                                     
039800                   TB1ACCE                                                
039900               SET                                                        
039910                  IDARTNR      = :ACCE-IDARTNR,                           
039920                  BEART        = :ACCE-BEART,                             
039930                  IDUPPDSU     = :ACCE-IDUPPDSU,                          
039931                  BEUPPDSU     = :ACCE-BEUPPDSU,                          
039940                  BEANST_SU    = :ACCE-BEANST-SU,                         
039950                  IDUPPDKU     = :ACCE-IDUPPDKU,                          
039951                  BEUPPDKU     = :ACCE-BEUPPDKU,                          
039960                  BEANST_KU    = :ACCE-BEANST-KU,                         
039970                  IDFKNGRP     = :ACCE-IDFKNGRP,                          
039980                  IDAOT        = :ACCE-IDAOT,                             
039990                  IDAOTUTG     = :ACCE-IDAOTUTG,                          
039991                  TIAOINF      = :ACCE-TIAOINF,                           
039992                  KDARTTYP     = :ACCE-KDARTTYP,                          
039993                  VKART_KDP    = :ACCE-VKART-KDP,                         
039994                  KDMDS        = :ACCE-KDMDS,                             
039995                  KDFARGST     = :ACCE-KDFARGST,                          
039996                  TEARTUTFG    = :ACCE-TEARTUTFG,                         
039997                  TESTATUPP    = :ACCE-TESTATUPP,                         
039998                  IDLEVNR_GSDB = :ACCE-IDLEVNR-GSDB,                      
039999                  KDTPD_PH1    = :ACCE-KDTPD-PH1,                         
040000                  DATPDPH1     = :ACCE-DATPDPH1,                          
040010                  DAPSWQP_1    = :ACCE-DAPSWQP-1,                         
040020                  KDPSWQP_1    = :ACCE-KDPSWQP-1,                         
040030                  DAPSWQA_1    = :ACCE-DAPSWQA-1,                         
040040                  KDPSWQA_1    = :ACCE-KDPSWQA-1,                         
040050                  DAPSWPP_2    = :ACCE-DAPSWPP-2,                         
040060                  KDPSWPP_2    = :ACCE-KDPSWPP-2,                         
040070                  DAPSWPA_2    = :ACCE-DAPSWPA-2,                         
040080                  KDPSWPA_2    = :ACCE-KDPSWPA-2,                         
040090                  DAPSWCP_3    = :ACCE-DAPSWCP-3,                         
040091                  KDPSWCP_3    = :ACCE-KDPSWCP-3,                         
040092                  DAPSWCA_3    = :ACCE-DAPSWCA-3,                         
040093                  KDPSWCA_3    = :ACCE-KDPSWCA-3,                         
040094                  TENOTE       = :ACCE-TENOTE,                            
040095                  FLANNULL     = :ACCE-FLANNULL,                          
040096                  IDKDPPOS     = :ACCE-IDKDPPOS,                          
040097                  IDPSLAG      = :ACCE-IDPSLAG,                           
040098                  IDTYPNR      = :ACCE-IDTYPNR,                           
040099                  BETYP        = :ACCE-BETYP,                             
040100                  IDARTNR_OFARG = :ACCE-IDARTNR-OFARG,                    
040110                  IDPROJK      = :ACCE-IDPROJK,                           
040120                  IDPSS        = :ACCE-IDPSS,                             
040121                  KDANNULL     = :ACCE-KDANNULL                           
040129                                                                          
041800               WHERE IDARTNR   = :W-IDARTNR                               
041810               AND   IDUPPDSU  = :W-IDUPPDSU                              
041900     END-EXEC                                                             
042000                                                                          
042100* COBOL                                                                   
042200     MOVE 000      TO GOOD-SQLCODESKODER                                  
042300     MOVE SQLCODE  TO SQLCODE-WS                                          
042400     PERFORM DB2-STATUSKONTROLL                                           
042500     .                                                                    
042600     EJECT                                                                
042700                                                                          
042800                                                                          
042900 DB2-INSERT-ROW-TB1ACCE SECTION.                                          
043000     MOVE 'DB2-INSERT-ROW-TB1ACCE ' TO DB2-SECTION-TEXT                   
043100* SQL - Lägger upp en ny tabellrad med 47 kolumner                        
043200     EXEC SQL  INSERT INTO                                                
043300                   TB1ACCE                                                
043400                   (                                                      
043500                   IDARTNR,                                               
043600                   BEART,                                                 
043800                   IDPRODGR,                                              
043900                   IDUPPDSU,                                              
043910                   BEUPPDSU,                                              
044000                   BEANST_SU,                                             
044100                   IDUPPDKU,                                              
044110                   BEUPPDKU,                                              
044200                   BEANST_KU,                                             
044300                   IDFKNGRP,                                              
044500                   IDAOT,                                                 
044600                   IDAOTUTG,                                              
044700                   TIAOINF,                                               
044800                   BEASSTYP,                                              
044900                   KDARTTYP,                                              
045000                   VKART_KDP,                                             
045100                   KDMDS,                                                 
045200                   KDFARGST,                                              
045300                   KDFRPTYP,                                              
045400                   TEARTUTFG,                                             
045410                   TESTATUPP,                                             
045420                   IDLEVNR_GSDB,                                          
045430                   KDTPD_PH1,                                             
045440                   DATPDPH1,                                              
045450                   DAPSWQP_1,                                             
045460                   KDPSWQP_1,                                             
045470                   DAPSWQA_1,                                             
045480                   KDPSWQA_1,                                             
045490                   DAPSWPP_2,                                             
045491                   KDPSWPP_2,                                             
045492                   DAPSWPA_2,                                             
045493                   KDPSWPA_2,                                             
045494                   DAPSWCP_3,                                             
045495                   KDPSWCP_3,                                             
045496                   DAPSWCA_3,                                             
045497                   KDPSWCA_3,                                             
045500                   KVYVOL_B3,                                             
045510                   KVYVOL_B2,                                             
045600                   KVYVOL,                                                
045800                   KVYVOL_INT,                                            
046100                   KVYVOL_B1,                                             
046200                   KVYVOL_ASS,                                            
046400                   KVMAM_N,                                               
046500                   KVMAM_E,                                               
046600                   KVMAM_O,                                               
047300                   BEMAPP,                                                
047700                   KVFOTO,                                                
047800                   TIFOTO,                                                
047900                   TENOTE,                                                
048000                   FLANNULL,                                              
048010                   IDKDPPOS,                                              
048020                   IDPSLAG,                                               
048030                   IDTYPNR,                                               
048040                   BETYP,                                                 
048050                   IDARTNR_OFARG,                                         
048060                   IDPROJK,                                               
048070                   IDPSS,                                                 
048080                   KDANNULL                                               
048100                   )                                                      
048200               VALUES                                                     
048300                   (                                                      
048400                   :ACCE-IDARTNR,                                         
048500                   :ACCE-BEART,                                           
048700                   :ACCE-IDPRODGR,                                        
048800                   :ACCE-IDUPPDSU,                                        
048810                   :ACCE-BEUPPDSU,                                        
048900                   :ACCE-BEANST-SU,                                       
049000                   :ACCE-IDUPPDKU,                                        
049010                   :ACCE-BEUPPDKU,                                        
049100                   :ACCE-BEANST-KU,                                       
049400                   :ACCE-IDFKNGRP,                                        
049410                   :ACCE-IDAOT,                                           
049500                   :ACCE-IDAOTUTG,                                        
049600                   :ACCE-TIAOINF,                                         
049700                   :ACCE-BEASSTYP,                                        
049800                   :ACCE-KDARTTYP,                                        
049900                   :ACCE-VKART-KDP,                                       
050000                   :ACCE-KDMDS,                                           
050100                   :ACCE-KDFARGST,                                        
050110                   :ACCE-KDFRPTYP,                                        
050120                   :ACCE-TEARTUTFG,                                       
050130                   :ACCE-TESTATUPP,                                       
050140                   :ACCE-IDLEVNR-GSDB,                                    
050200                   :ACCE-KDTPD-PH1,                                       
050300                   :ACCE-DATPDPH1,                                        
050310                   :ACCE-DAPSWQP-1,                                       
050320                   :ACCE-KDPSWQP-1,                                       
050330                   :ACCE-DAPSWQA-1,                                       
050340                   :ACCE-KDPSWQA-1,                                       
050350                   :ACCE-DAPSWPP-2,                                       
050360                   :ACCE-KDPSWPP-2,                                       
050370                   :ACCE-DAPSWPA-2,                                       
050380                   :ACCE-KDPSWPA-2,                                       
050390                   :ACCE-DAPSWCP-3,                                       
050391                   :ACCE-KDPSWCP-3,                                       
050392                   :ACCE-DAPSWCA-3,                                       
050393                   :ACCE-KDPSWCA-3,                                       
050400                   :ACCE-KVYVOL-B3,                                       
050410                   :ACCE-KVYVOL-B2,                                       
050500                   :ACCE-KVYVOL,                                          
050700                   :ACCE-KVYVOL-INT,                                      
051000                   :ACCE-KVYVOL-B1,                                       
051100                   :ACCE-KVYVOL-ASS,                                      
051300                   :ACCE-KVMAM-N,                                         
051400                   :ACCE-KVMAM-E,                                         
051500                   :ACCE-KVMAM-O,                                         
052200                   :ACCE-BEMAPP,                                          
052600                   :ACCE-KVFOTO,                                          
052700                   :ACCE-TIFOTO,                                          
052800                   :ACCE-TENOTE,                                          
052900                   :ACCE-FLANNULL,                                        
052910                   :ACCE-IDKDPPOS,                                        
052920                   :ACCE-IDPSLAG,                                         
052930                   :ACCE-IDTYPNR,                                         
052940                   :ACCE-BETYP,                                           
052950                   :ACCE-IDARTNR-OFARG,                                   
052960                   :ACCE-IDPROJK,                                         
052970                   :ACCE-IDPSS,                                           
052980                   :ACCE-KDANNULL                                         
053000                   )                                                      
053100     END-EXEC                                                             
053200                                                                          
053300     MOVE 000      TO GOOD-SQLCODESKODER                                  
053400     MOVE SQLCODE  TO SQLCODE-WS                                          
053500     PERFORM DB2-STATUSKONTROLL                                           
053600     .                                                                    
053700     EJECT                                                                
053800 DB2-STATUSKONTROLL SECTION.                                              
053900     MOVE 'DB2-STATUSKONTROLL     ' TO DB2-SECTION-TEXT                   
054000                                                                          
054100     SET SQLCODE-IX         TO 1                                          
054200     SEARCH GOOD-SQLCODE AT END                                           
054300           CALL ABEND USING RKOD-ABEND-DB2                                
054400        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
054500           CONTINUE                                                       
054600     END-SEARCH                                                           
054700     .                                                                    
