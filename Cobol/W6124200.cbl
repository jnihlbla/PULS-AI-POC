000100*                                                                         
000200******************************************************************        
000300*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL1003      *        
000400******************************************************************        
000500*                                                                         
000600 ID DIVISION.                                                             
000700 PROGRAM-ID.     W6124200.                                                
000800 AUTHOR.         BODIL LINDAHL.                                           
000900 DATE-WRITTEN.   99/07/13.                                                
001000 DATE-COMPILED.                                                           
001100                                                                          
001200*    FUNKTION:                                                            
001300*        LÄSER FIL W612PP MED URVAL FRÅN BILD 6302                        
001400*        SKAPAR UTFIL TILL LISTA (BINNINGLIST)                            
001500*                                                                         
001600*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001700*                              WLINLD (WDL6A)                             
001800*                                      WDK7                               
001900*                                      WDK6                               
002000*                              W6KVAH (W6D2)                              
002100*                              WLBENA (WDD3)                              
002200*                              WL6301 (WDR5)                              
002300*                                      WDB6                               
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*          --- URVAL FRÅN 6302                                            
003400     SELECT W612PP                     ASSIGN TO W61242D1.                
003500     SKIP2                                                                
003600*          --- UTFIL FÖR LISTA                                            
003700     SELECT UTFIL                      ASSIGN TO W61242D2.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W612PP                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700 01  PARM             PIC X(80).                                          
004800     EJECT                                                                
004900 FD  UTFIL                                                                
005000     RECORDING       V                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  UT1-POST      -COPY W612421    -L.                                   
005400*01  UT2-POST      -COPY W612422    -L.                                   
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800                                                                          
005900*    -- CHECKED BY WY2000                                                 
006000 77  IDPGM                       PIC X(8)    VALUE 'W6124200'.            
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300 77  WS-KVROS                    PIC S9(7)   VALUE ZERO COMP-3.           
006400 77  WS-PTYP01-SKAPAD            PIC X       VALUE 'N'.                   
006500 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
006600 77  PARM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
006700 77  PARM-IX-MAX                 PIC S9(3)   VALUE +12  COMP-3.           
006800 77  MAX-IX                      PIC S9(3)   VALUE +7   COMP-3.           
006900                                                                          
007000 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
007100     88  END-OF-W612PP                       VALUE 'J'.                   
007200                                                                          
007800 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
007900 01  FILLER REDEFINES DAGENS-DATUM.                                       
008000     03  DAGENS-DATUM-SEKEL      PIC 9(2).                                
008100     03  DAGENS-DATUM-AAMMDD     PIC 9(6).                                
008200     SKIP3                                                                
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400*                                                                         
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008900     SKIP2                                                                
009000 01  FELTEXT.                                                             
009100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009300     EJECT                                                                
009400*      --- VALID IDDC CODES                                               
009500*                                                                         
009600*01    -COPY WWDC99                                                       
009700       EJECT                                                              
009800*    --- PARAMETRAR TILL POSTSUM                                          
009900*01  -COPY W0005   -PRE  POSTSUM-                                         
010000     EJECT                                                                
010400 01  PARM-AREA-START             PIC X(24)   VALUE                        
010500                                 'PARM-AREA-START '.                      
010600 01  PARM-AREA                   PIC X(500).                              
010700 01  FILLER REDEFINES PARM-AREA.                                          
010800     03 PARM-TABELL.                                                      
010900        05 PARM-TAB-RAD OCCURS 12.                                        
011000           07 PARM-CMD           PIC X(3).                                
011100           07 PARM-IDFAKT        PIC 9(7).                                
011200           07 PARM-IDKOLLI       PIC 9(5).                                
011300           07 PARM-IDKUNDRF      PIC X(10).                               
011400           07 PARM-IDKUNDNR      PIC 9(7).                                
011500           07 PARM-KDMATT        PIC X.                                   
011600     EJECT                                                                
011700 01  UT-AREA-START               PIC X(24)   VALUE                        
011800                                 'UT-AREA-START '.                        
011900 01  UT-AREA                     PIC X(621).                              
012000 01  PTYP1-AREA REDEFINES UT-AREA.                                        
012100*    03  -COPY W612421 -PRE UT01-                                         
012200     EJECT                                                                
012300 01  PTYP2-AREA REDEFINES UT-AREA.                                        
012400*    03  -COPY W612422 -PRE UT02-                                         
012500     EJECT                                                                
012600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900     SKIP3                                                                
013000 01  NYCKLAR-TILL-DLI.                                                    
013100     03  W-IDARTNR-X.                                                     
013200         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
013300     03  W-KDSEGKEY-X.                                                    
013400         05  W-KDSEGKEY          PIC X(1)   VALUE '1'.                    
013500     03  W-IDDC-REF-X.                                                    
013600         05  W-IDDC-REF          PIC X(2)   VALUE SPACE.                  
013700                                                                          
013800     03  W-DAINLEV-X.                                                     
013900         05  W-DAINLEV           PIC 9(16)  VALUE ZERO.                   
014000     03  W-IDFAKT-X.                                                      
014100         05  W-IDFAKT            PIC S9(7)  VALUE ZERO COMP-3.            
014200     03  W-IDDC-X.                                                        
014300         05  W-IDDC              PIC X(2)   VALUE SPACE.                  
014400     03  W-IDLAND-X.                                                      
014500         05  W-IDLAND            PIC X(2)   VALUE SPACE.                  
014600     03  W-IDSKYLT-X.                                                     
014700         05  W-IDSKYLT           PIC X(3)   VALUE 'GB '.                  
014800     03  W-6301KEY-X.                                                     
014900         05  W-6301-IDHTYP       PIC X(4)   VALUE '6301'.                 
015000         05  W-6301-IDDC         PIC X(2)   VALUE SPACE.                  
015100         05  FILLER              PIC X(24)  VALUE LOW-VALUE.              
015200     03  W-WDL6A1KY-MIN.                                                  
015300         05  W-IDFAKT-MIN         PIC S9(7) COMP-3.                       
015400         05  W-IDKUNDRF-MIN       PIC X(10).                              
015500         05  W-IDKUNDNR-MIN       PIC S9(7) COMP-3.                       
015600         05  W-IDKOLLI-MIN        PIC S9(5) COMP-3.                       
015700         05  FILLER               PIC X(21).                              
015800     03  W-WDL6A1KY-MAX.                                                  
015900         05  W-IDFAKT-MAX         PIC S9(7) COMP-3.                       
016000         05  W-IDKUNDRF-MAX       PIC X(10).                              
016100         05  W-IDKUNDNR-MAX       PIC S9(7) COMP-3.                       
016200         05  W-IDKOLLI-MAX        PIC S9(5) COMP-3.                       
016300         05  FILLER               PIC X(21).                              
016310     03  W-W6D211KY-X.                                                    
016320         05  W-DAREGDAT-9KOMPL   PIC 9(8)   VALUE 79000001.               
016330         05  W-TIKLOCK-9KOMPL    PIC S9(9)  VALUE ZERO COMP-3.            
016400     EJECT                                                                
016500*    --- STATUS-KOD FRÅN IMS                                              
016600 01  STATUS-WS                   PIC XX.                                  
016700     88  SEGMENT-FINNS                       VALUE '  '.                  
016800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017000     SKIP2                                                                
017100 01  GODK-STATUSKODER.                                                    
017200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017300     SKIP3                                                                
017400 01  SSA1                        PIC X(128).                              
017500 01  SSA2                        PIC X(64).                               
017600 01  SSA3                        PIC X(64).                               
017700     EJECT                                                                
017800*    --- IMS FUNKTIONSKODER                                               
017900*01  -COPY W0003                                                          
018000     EJECT                                                                
018100*    ---  DLI INPUT-OUTPUT AREA                                           
018200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC'.                      
018300 01  DLI-IO-WLINLC.                                                       
018400*    03  -COPY WDL611                                                     
018500     EJECT                                                                
018600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
018700 01  DLI-IO-WDK711.                                                       
018800*    03  -COPY WDK711                                                     
018900     EJECT                                                                
019000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
019100 01  DLI-IO-WDK712.                                                       
019200*    03  -COPY WDK712                                                     
019300     EJECT                                                                
019400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLD'.                      
019500 01  DLI-IO-WLINLD.                                                       
019600*    03  -COPY WDL6A1                                                     
019700     EJECT                                                                
019800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLGX63'.                      
019900 01  DLI-IO-WLGX63.                                                       
020000*    03  -COPY WDGX6302                                                   
020100     EJECT                                                                
020200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
020300 01  DLI-IO-WDK611.                                                       
020400*    03  -COPY WDK611                                                     
020500     EJECT                                                                
020600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
020700 01  DLI-IO-WDK629.                                                       
020800*    03  -COPY WDK629                                                     
020900     EJECT                                                                
021000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLBENA'.                      
021100 01  DLI-IO-WLBENA.                                                       
021200*    03  -COPY WDD311                                                     
021300     EJECT                                                                
021400 01  FILLER         PIC X(24) VALUE 'DLI-IO-W6KVAH'.                      
021500 01  DLI-IO-W6KVAH.                                                       
021600*    03  -COPY W6D211                                                     
021700     EJECT                                                                
021800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601'.                      
021900 01  DLI-IO-WDB601.                                                       
022000*    03  -COPY WDB601                                                     
022100     EJECT                                                                
022200 LINKAGE SECTION.                                                         
022300                                                                          
022400*01  -COPY W0008  -PRE INLC-                                              
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700*01  -COPY W0008  -PRE WDK7-                                              
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000*01  -COPY W0008  -PRE INLD-                                              
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01  -COPY W0008  -PRE GX63-                                              
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600*01  -COPY W0008  -PRE WDK6-                                              
023700     05  FILLER                  PIC X.                                   
023800     EJECT                                                                
023900*01  -COPY W0008  -PRE BENA-                                              
024000     05  FILLER                  PIC X.                                   
024100     EJECT                                                                
024200*01  -COPY W0008  -PRE KVAH-                                              
024300     05  FILLER                  PIC X.                                   
024400     EJECT                                                                
024500*01  -COPY W0008  -PRE WDB6-                                              
024600     05  FILLER                  PIC X.                                   
024700     EJECT                                                                
024800 PROCEDURE DIVISION  USING INLC-PCB WDK7-PCB INLD-PCB GX63-PCB            
024900                           WDK6-PCB BENA-PCB KVAH-PCB WDB6-PCB.           
025000 MAIN SECTION.                                                            
025100     ENTRY 'DLITCBL' USING INLC-PCB WDK7-PCB INLD-PCB GX63-PCB            
025200                           WDK6-PCB BENA-PCB KVAH-PCB WDB6-PCB.           
025300                                                                          
025400     PERFORM A-INIT                                                       
025500                                                                          
025600     PERFORM S11-LAES-W612PP                                              
025700     PERFORM B-SKAPA-UTFIL                                                
025800                                                                          
025900     PERFORM Z-FINIT                                                      
026000                                                                          
026100     MOVE ZERO TO RETURN-CODE                                             
026200     GOBACK                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 A-INIT SECTION.                                                          
026600                                                                          
026700     OPEN INPUT  W612PP                                                   
026800     OPEN OUTPUT UTFIL                                                    
026900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027000     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
027100                                                                          
027200     MOVE NEJ        TO WS-PTYP01-SKAPAD                                  
027300     MOVE LOW-VALUE  TO W-WDL6A1KY-MIN                                    
027400     MOVE HIGH-VALUE TO W-WDL6A1KY-MAX                                    
027500     PERFORM S01-NOLLSTALL                                                
027600     .                                                                    
027700     EJECT                                                                
027800 B-SKAPA-UTFIL SECTION.                                                   
027900                                                                          
028000     MOVE +1 TO PARM-IX                                                   
028100     PERFORM UNTIL PARM-IX > PARM-IX-MAX                                  
028200                                                                          
028300        IF PARM-IDFAKT(PARM-IX) NUMERIC                                   
028400        AND PARM-IDKUNDNR(PARM-IX) NUMERIC                                
028500        AND PARM-IDKOLLI(PARM-IX) NUMERIC                                 
028600           IF PARM-IDFAKT(PARM-IX) NOT = ZERO                             
028700              MOVE PARM-IDFAKT(PARM-IX)   TO W-IDFAKT-MIN                 
028800                                             W-IDFAKT-MAX                 
028900                                             W-IDFAKT                     
029000              MOVE PARM-IDKUNDRF(PARM-IX) TO W-IDKUNDRF-MIN               
029100                                             W-IDKUNDRF-MAX               
029200              MOVE PARM-IDKUNDNR(PARM-IX) TO W-IDKUNDNR-MIN               
029300                                             W-IDKUNDNR-MAX               
029400              MOVE PARM-IDKOLLI(PARM-IX)  TO W-IDKOLLI-MIN                
029500                                             W-IDKOLLI-MAX                
029600                                                                          
029700              PERFORM IMS-GU-WLINLD01                                     
029800              PERFORM UNTIL SEGMENT-SAKNAS                                
029900                                                                          
030000                 IF WS-PTYP01-SKAPAD = NEJ                                
030100                    MOVE SEQA-IDDC TO W-6301-IDDC                         
030200                                      W-IDDC                              
030300                                      WS-IDDC                             
030400                    PERFORM BA-SKAPA-UTPOST-PTYP01                        
030500                    PERFORM S12-SKRIV-UTPOST-PTYP01                       
030600                    PERFORM S01-NOLLSTALL                                 
030700                    MOVE JA TO WS-PTYP01-SKAPAD                           
030800                 END-IF                                                   
030900                                                                          
031000                 MOVE SEQA-IDARTNR TO W-IDARTNR                           
031100                 MOVE SEQA-DAINLEV TO W-DAINLEV                           
031200                 PERFORM IMS-GET-WLINLC11                                 
031300                 PERFORM BB-SKAPA-UTPOST-PTYP02                           
031400                 PERFORM S13-SKRIV-UTPOST-PTYP02                          
031500                 PERFORM S01-NOLLSTALL                                    
031600                                                                          
031700                 PERFORM IMS-GN-WLINLD01                                  
031800              END-PERFORM                                                 
031900           END-IF                                                         
032000        END-IF                                                            
032100        ADD +1 TO PARM-IX                                                 
032200        MOVE NEJ TO WS-PTYP01-SKAPAD                                      
032300     END-PERFORM                                                          
032400     .                                                                    
032500     EJECT                                                                
032600 BA-SKAPA-UTPOST-PTYP01 SECTION.                                          
032700                                                                          
032800     MOVE '001'                TO UT01-IDPTYP                             
032900     MOVE PARM-CMD(PARM-IX)    TO UT01-CMD                                
033000     MOVE PARM-KDMATT(PARM-IX) TO UT01-KDMATT                             
033100     MOVE SEQA-IDDC            TO UT01-IDDC                               
033200     MOVE SEQA-IDFAKT          TO UT01-IDFAKT                             
033300     MOVE SEQA-IDKUNDRF        TO UT01-IDKUNDRF                           
033400     MOVE SEQA-IDKUNDNR        TO UT01-IDKUNDNR                           
033500     MOVE SEQA-IDKOLLI         TO UT01-IDKOLLI                            
033600                                                                          
033700     PERFORM IMS-GET-WL630111                                             
033800     MOVE 6302-IDDISTR  TO UT01-IDDISTR                                   
033900     MOVE 6302-DABERANK TO UT01-DABERANK                                  
034000     .                                                                    
034100     EJECT                                                                
034200 BB-SKAPA-UTPOST-PTYP02 SECTION.                                          
034300                                                                          
034400     MOVE '002'          TO UT02-IDPTYP                                   
034500     MOVE INL-FLPRIO     TO UT02-FLPRIO                                   
034600     MOVE INL-KVAVIS     TO UT02-KVAVIS                                   
034700     MOVE SEQA-IDARTNR   TO UT02-IDARTNR                                  
034800                            W-IDARTNR                                     
034900                                                                          
035000     PERFORM IMS-GET-WDK611                                               
035100                                                                          
035200     IF CDC-SE                                                            
035300        IF CLAG-ADART = ZERO                                              
035400           MOVE ZERO             TO UT02-ADLAGOMR                         
035500                                    UT02-ADGANG                           
035600                                    UT02-ADGANG                           
035700        ELSE                                                              
035800           MOVE CLAG-ADLAGOMR    TO UT02-ADLAGOMR                         
035900           MOVE CLAG-ADGANG      TO UT02-ADGANG                           
036000           MOVE CLAG-ADPLATS     TO UT02-ADPLATS                          
036100        END-IF                                                            
036200        MOVE CLAG-KDARTURS       TO UT02-KDARTURS                         
036300        MOVE CLAG-IDDC-REF       TO W-IDDC-REF                            
036400        MOVE CLAG-KVROS          TO WS-KVROS                              
036500        PERFORM IMS-GET-WDK629                                            
036600        IF SEGMENT-FINNS                                                  
036700           MOVE CREF-IDPERSON-BUY                                         
036800                                 TO UT02-IDPERSON-BUY                     
036900        END-IF                                                            
037000     ELSE                                                                 
037100        PERFORM IMS-GET-WDK711                                            
037200        IF SLAG-ADART = ZERO                                              
037300           MOVE ZERO             TO UT02-ADLAGOMR                         
037400                                    UT02-ADGANG                           
037500                                    UT02-ADGANG                           
037600        ELSE                                                              
037700           MOVE SLAG-ADLAGOMR    TO UT02-ADLAGOMR                         
037800           MOVE SLAG-ADGANG      TO UT02-ADGANG                           
037900           MOVE SLAG-ADPLATS     TO UT02-ADPLATS                          
038000        END-IF                                                            
038100        IF SLAG-IDLEVNR = '1441 '                                         
038200        OR SLAG-IDLEVNR = SPACE                                           
038300        OR SLAG-IDLEVNR = 'BP2TW'                                         
038400           MOVE SPACE            TO UT02-KDARTURS                         
038500        ELSE                                                              
038600           IF DCS-IDDC NOT = W-IDDC                                       
038700              PERFORM IMS-GET-WDB601                                      
038800              MOVE DCS-IDLANDX2  TO W-IDLAND                              
038900              PERFORM IMS-GET-WDK712                                      
039000              IF SEGMENT-FINNS                                            
039100                 MOVE LART-KDARTURS                                       
039200                                 TO UT02-KDARTURS                         
039300              END-IF                                                      
039400           END-IF                                                         
039500        END-IF                                                            
039600        COMPUTE WS-KVROS = SLAG-KVROS-DAG + SLAG-KVROS-BULK               
039700        MOVE SLAG-IDPERSON-BUY   TO UT02-IDPERSON-BUY                     
039800     END-IF                                                               
039900                                                                          
040000     IF WS-KVROS >= INL-KVAVIS                                            
040100        MOVE 'Y'         TO UT02-FLBACKORDER                              
040200     ELSE                                                                 
040300        MOVE 'N'         TO UT02-FLBACKORDER                              
040400     END-IF                                                               
040500                                                                          
040600     IF CLAG-KDFARLIG = 4                                                 
040700     OR CLAG-KDFARLIG = 7                                                 
040800        MOVE 'Y'            TO UT02-FLKDFARLIG                            
040900     ELSE                                                                 
041000        MOVE 'N'            TO UT02-FLKDFARLIG                            
041100     END-IF                                                               
041200     IF UT02-KDARTURS = SPACE                                             
041300        MOVE CLAG-KDARTURS  TO UT02-KDARTURS                              
041400     END-IF                                                               
041500     MOVE CLAG-VKART        TO UT02-VKART                                 
041600     MOVE CLAG-VLARTNTO     TO UT02-VLARTNTO                              
041700                                                                          
041800                                                                          
041900     PERFORM IMS-GET-BENA11                                               
042000     MOVE TEXT-BEART        TO UT02-BEART                                 
042100                                                                          
042300     PERFORM IMS-GET-KVAH11                                               
042400     IF SEGMENT-FINNS                                                     
042600                                                                          
042700        IF INFO-DAREGDAT-9KOMPL > INL-DAINLEV(1:8)                        
043000**SHIPMENT IS DONE AT LEAST ONE DAY AFTER THE QUALITY REMARK              
043010           CONTINUE                                                       
043100        ELSE                                                              
043710           COMPUTE UT02-DAREGDAT = 99999999 - INFO-DAREGDAT-9KOMPL        
043800           MOVE +1 TO IX                                                  
043900           PERFORM UNTIL IX > MAX-IX                                      
044000              MOVE INFO-TEKVAINF-EXT(IX)                                  
044100                   TO UT02-TEKVAINF-EXT(IX)                               
044200              INSPECT UT02-TEKVAINF-EXT(IX) REPLACING                     
044300                   ALL '#' BY SPACE                                       
044400              INSPECT UT02-TEKVAINF-EXT(IX) REPLACING                     
044500                   ALL 'Å' BY 'A'                                         
044600              INSPECT UT02-TEKVAINF-EXT(IX) REPLACING                     
044700                   ALL 'Ä' BY 'A'                                         
044800              INSPECT UT02-TEKVAINF-EXT(IX) REPLACING                     
044900                   ALL 'Ö' BY 'O'                                         
045000                   ADD +1 TO IX                                           
045100           END-PERFORM                                                    
045300        END-IF                                                            
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800 Z-FINIT SECTION.                                                         
045900                                                                          
046000     CLOSE W612PP                                                         
046100           UTFIL                                                          
046200                                                                          
046300     MOVE 'S' TO POSTSUM-OPKOD                                            
046400     CALL POSTSUM USING POSTSUM-PARM                                      
046500     .                                                                    
046600     EJECT                                                                
046700 S01-NOLLSTALL SECTION.                                                   
046800                                                                          
046900     MOVE SPACE TO UT01-IDPTYP                                            
047000                   UT01-CMD                                               
047100                   UT01-IDDC                                              
047200                   UT01-KDMATT                                            
047300                   UT01-IDKUNDRF                                          
047400                   UT02-IDPTYP                                            
047500                   UT02-FLPRIO                                            
047600                   UT02-FLBACKORDER                                       
047700                   UT02-FLKDFARLIG                                        
047800                   UT02-KDARTURS                                          
047900                   UT02-BEART                                             
048000                   UT02-TEKVAINF-EXT(1)                                   
048100                   UT02-TEKVAINF-EXT(2)                                   
048200                   UT02-TEKVAINF-EXT(3)                                   
048300                   UT02-TEKVAINF-EXT(4)                                   
048400                   UT02-TEKVAINF-EXT(5)                                   
048500                   UT02-TEKVAINF-EXT(6)                                   
048600                   UT02-TEKVAINF-EXT(7)                                   
048700     MOVE ZERO TO  UT01-IDFAKT                                            
048800                   UT01-IDKUNDNR                                          
048900                   UT01-IDKOLLI                                           
049000                   UT01-IDDISTR                                           
049100                   UT01-DABERANK                                          
049200                   UT02-IDARTNR                                           
049300                   UT02-IDPERSON-BUY                                      
049400                   UT02-ADLAGOMR                                          
049500                   UT02-ADGANG                                            
049600                   UT02-ADPLATS                                           
049700                   UT02-KVAVIS                                            
049800                   UT02-VKART                                             
049900                   UT02-VLARTNTO                                          
050000                   UT02-DAREGDAT                                          
050100     .                                                                    
050200     EJECT                                                                
050300 S11-LAES-W612PP SECTION.                                                 
050400                                                                          
050500     READ W612PP INTO PARM-TAB-RAD(1)                                     
050600     READ W612PP INTO PARM-TAB-RAD(2)                                     
050700     READ W612PP INTO PARM-TAB-RAD(3)                                     
050800     READ W612PP INTO PARM-TAB-RAD(4)                                     
050900     READ W612PP INTO PARM-TAB-RAD(5)                                     
051000     READ W612PP INTO PARM-TAB-RAD(6)                                     
051100     READ W612PP INTO PARM-TAB-RAD(7)                                     
051200     READ W612PP INTO PARM-TAB-RAD(8)                                     
051300     READ W612PP INTO PARM-TAB-RAD(9)                                     
051400     READ W612PP INTO PARM-TAB-RAD(10)                                    
051500     READ W612PP INTO PARM-TAB-RAD(11)                                    
051600     READ W612PP INTO PARM-TAB-RAD(12)                                    
051700     .                                                                    
051800     EJECT                                                                
051900 S12-SKRIV-UTPOST-PTYP01 SECTION.                                         
052000                                                                          
052100     WRITE UT1-POST FROM UT-AREA                                          
052200                                                                          
052300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
052400     MOVE 'UTFIL  '  TO POSTSUM-FDNAMN                                    
052500     MOVE 'W61242D2' TO POSTSUM-DDNAMN2                                   
052600     CALL POSTSUM USING POSTSUM-PARM                                      
052700     .                                                                    
052800     EJECT                                                                
052900 S13-SKRIV-UTPOST-PTYP02 SECTION.                                         
053000                                                                          
053100     WRITE UT2-POST FROM UT-AREA                                          
053200                                                                          
053300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
053400     MOVE 'UTFIL  '  TO POSTSUM-FDNAMN                                    
053500     MOVE 'W61242D2' TO POSTSUM-DDNAMN2                                   
053600     CALL POSTSUM USING POSTSUM-PARM                                      
053700     .                                                                    
053800     EJECT                                                                
053900* --- IMS SEKTIONER ---                                                   
054000     SKIP3                                                                
054100 IMS-GET-WLINLC11   SECTION.                                              
054200     STRING 'WLINLC01(IDARTNR = ' W-IDARTNR-X ')'                         
054300          DELIMITED BY SIZE INTO SSA1                                     
054400     STRING 'WLINLC11(DAINLEV = ' W-DAINLEV-X ')'                         
054500          DELIMITED BY SIZE INTO SSA2                                     
054600     MOVE SPACE  TO GODK-STATUSKODER                                      
054700     CALL CBLTDLI USING GU INLC-PCB DLI-IO-WLINLC SSA1 SSA2               
054800     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
054900     PERFORM IMS-STATUSKONTROLL                                           
055000     .                                                                    
055100     SKIP3                                                                
055200 IMS-GU-WLINLD01 SECTION.                                                 
055300     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
055400                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
055500          DELIMITED BY SIZE INTO SSA1                                     
055600     MOVE '  GE' TO GODK-STATUSKODER                                      
055700     CALL CBLTDLI USING GU INLD-PCB DLI-IO-WLINLD SSA1                    
055800     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
055900     PERFORM IMS-STATUSKONTROLL                                           
056000     .                                                                    
056100     SKIP3                                                                
056200 IMS-GN-WLINLD01 SECTION.                                                 
056300     STRING 'WLINLD01(WDL6A1KY=>' W-WDL6A1KY-MIN                          
056400                    '&WDL6A1KY=<' W-WDL6A1KY-MAX ')'                      
056500          DELIMITED BY SIZE INTO SSA1                                     
056600     MOVE '  GE' TO GODK-STATUSKODER                                      
056700     CALL CBLTDLI USING GN INLD-PCB DLI-IO-WLINLD SSA1                    
056800     MOVE INLD-STATUS-CODE TO STATUS-WS                                   
056900     PERFORM IMS-STATUSKONTROLL                                           
057000     .                                                                    
057100     EJECT                                                                
057200 IMS-GET-WDK611 SECTION.                                                  
057300     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
057400          DELIMITED BY SIZE INTO SSA1                                     
057500     MOVE 'WDK611   ' TO SSA2                                             
057600     MOVE '  GE' TO GODK-STATUSKODER                                      
057700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
057800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
057900     PERFORM IMS-STATUSKONTROLL                                           
058000     .                                                                    
058100     SKIP3                                                                
058200 IMS-GET-WDK629 SECTION.                                                  
058300     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
058400          DELIMITED BY SIZE INTO SSA1                                     
058500     STRING 'WDK611  (KDSEGKEY= ' W-KDSEGKEY-X ')'                        
058600          DELIMITED BY SIZE INTO SSA2                                     
058700     STRING 'WDK629  (IDDCREF = ' W-IDDC-REF-X ')'                        
058800          DELIMITED BY SIZE INTO SSA3                                     
058900     MOVE '  GE' TO GODK-STATUSKODER                                      
059000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK629 SSA1 SSA2 SSA3          
059100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
059200     PERFORM IMS-STATUSKONTROLL                                           
059300     .                                                                    
059400     SKIP3                                                                
059500 IMS-GET-WDK711 SECTION.                                                  
059600     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
059700          DELIMITED BY SIZE INTO SSA1                                     
059800     STRING 'WDK711  (IDDC    = ' W-IDDC  ')'                             
059900          DELIMITED BY SIZE INTO SSA2                                     
060000     MOVE '  GE' TO GODK-STATUSKODER                                      
060100     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
060200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
060300     PERFORM IMS-STATUSKONTROLL                                           
060400     .                                                                    
060500     SKIP3                                                                
060600 IMS-GET-WDK712 SECTION.                                                  
060700     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
060800          DELIMITED BY SIZE INTO SSA1                                     
060900     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
061000          DELIMITED BY SIZE INTO SSA2                                     
061100     MOVE '  GE' TO GODK-STATUSKODER                                      
061200     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
061300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
061400     PERFORM IMS-STATUSKONTROLL                                           
061500     .                                                                    
061600     SKIP3                                                                
061700 IMS-GET-WL630111 SECTION.                                                
061800     STRING 'WL630101(WDGXKEY = ' W-6301KEY-X ')'                         
061900          DELIMITED BY SIZE INTO SSA1                                     
062000     STRING 'WL630111(IDFAKT  = ' W-IDFAKT-X ')'                          
062100          DELIMITED BY SIZE INTO SSA2                                     
062200     MOVE SPACE TO GODK-STATUSKODER                                       
062300     CALL CBLTDLI USING GU GX63-PCB DLI-IO-WLGX63 SSA1 SSA2               
062400     MOVE GX63-STATUS-CODE TO STATUS-WS                                   
062500     PERFORM IMS-STATUSKONTROLL                                           
062600     EJECT                                                                
062700     .                                                                    
062800 IMS-GET-BENA11 SECTION.                                                  
062900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
063000             DELIMITED BY SIZE INTO SSA1                                  
063100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
063200              DELIMITED BY SIZE INTO SSA2                                 
063300     MOVE '  GE' TO GODK-STATUSKODER                                      
063400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA SSA1 SSA2               
063500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
063600     PERFORM IMS-STATUSKONTROLL                                           
063700     .                                                                    
063800     SKIP3                                                                
063900 IMS-GET-KVAH11 SECTION.                                                  
064000     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
064100             DELIMITED BY SIZE INTO SSA1                                  
064210     STRING 'W6KVAH11(W6D211KY >' W-W6D211KY-X ')'                        
064220             DELIMITED BY SIZE INTO SSA2                                  
064300     MOVE '  GE' TO GODK-STATUSKODER                                      
064400     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-W6KVAH SSA1 SSA2               
064500     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
064600     PERFORM IMS-STATUSKONTROLL                                           
064700     .                                                                    
064800     SKIP3                                                                
064900 IMS-GET-WDB601       SECTION.                                            
065000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
065100             DELIMITED BY SIZE INTO SSA1                                  
065200     MOVE '    ' TO GODK-STATUSKODER                                      
065300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
065400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
065500     PERFORM IMS-STATUSKONTROLL                                           
065600     .                                                                    
065700     EJECT                                                                
065800 IMS-STATUSKONTROLL SECTION.                                              
065900                                                                          
066000     SET STATUS-IX TO 1                                                   
066100     SEARCH GODK-STATUS                                                   
066200       AT END                                                             
066300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
066400           DELIMITED BY SIZE INTO FELTEXT                                 
066500         DISPLAY FELTEXT                                                  
066600         CALL FELLOG                                                      
066700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066800         CONTINUE                                                         
066900     END-SEARCH                                                           
067000     .                                                                    
