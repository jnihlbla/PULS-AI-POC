000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4038500.                                                
000400 AUTHOR.         CAO-VAN NGU.                                             
000500 DATE-WRITTEN.   90/06/07.                                                
000600*                                                                         
000700*    REMARKS.                                                             
000800*    DISPLAY PLANNED ORDERS INFORMATION ON SCREEN 4385                    
000900*    INDATA.                                                              
001000*        TRANSAKTION: W4T385                                              
001100*        MID:         W4I38501                                            
001200*    UTDATA.                                                              
001300*        MOD:         W4O38501                                            
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     EJECT                                                                
001800 DATA DIVISION.                                                           
001900 WORKING-STORAGE SECTION.                                                 
001901                                                                          
001910*    -- CHECKED BY WY2000                                                 
002000 77  IDPGM                       PIC X(08)   VALUE 'W4038500'.            
002100                                                                          
002200 77  JA                          PIC X       VALUE 'J'.                   
002300 77  NEJ                         PIC X       VALUE 'N'.                   
002400                                                                          
002500*    ---                                                                  
002600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
002700 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
002800                                                                          
002900 77  W-KDORDSTA                  PIC X(2)   VALUE SPACE.                  
003000 77  WS-IDTIDZON                 PIC X(2)   VALUE SPACE.                  
003010 77  WS-VKORDNTO                 PIC S9(6)V9(1) COMP-3.                   
003020 77  WS-VLORDNTO                 PIC S9(4)V9(3) COMP-3.                   
003100                                                                          
003200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003300     88  NYCKLAR-OK                          VALUE 'J'.                   
003400     88  NYCKLAR-FEL                         VALUE 'N'.                   
003500                                                                          
003600 77  TRP-SW                      PIC X.                                   
003700     88  TRP-3                               VALUE '3'.                   
003800     88  TRP-5                               VALUE '5'.                   
003900                                                                          
004000 77  DELKVAL-SW                  PIC X       VALUE 'N'.                   
004100     88  DELKVAL-OK                          VALUE 'J'.                   
004200                                                                          
004300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004400     88  ALLT-OK                             VALUE 'J'.                   
004500                                                                          
004600 77  KDORDSTA-SW                 PIC X       VALUE 'N'.                   
004700     88  KDORDSTA-OK                         VALUE 'J'.                   
004800                                                                          
004900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005000     88  EGEN-MID                            VALUE '4385'.                
005100     88  GODK-MID                            VALUE '4385'.                
005200                                                                          
005300 01      WS-TIRFS                PIC 9(11).                               
005400 01      FILLER REDEFINES WS-TIRFS.                                       
005500   03    FILLER                  PIC X(1).                                
005600   03    WS-RFS-DATE             PIC 9(6).                                
005700   03    WS-RFS-TIME             PIC X(4).                                
005800                                                                          
005900 01  WS-KDMATT                   PIC X.                                   
006000     88 US-MEASUREMENT           VALUE 'U'.                               
006100     88 SIS-MEASUREMENT          VALUE 'S'.                               
006200*                                                                         
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007110     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
007200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007300*01 -COPY WMSGINIT                                                        
007800     SKIP3                                                                
007810 01  FILLER                      PIC X(16) VALUE 'WWOMVAND-AREA'.         
007820*01  -COPY WWOMVAND                                                       
007830     SKIP3                                                                
007900*    ---                                                                  
008000*   -COPY WMEDAREA                                                        
008100     EJECT                                                                
008200*   -COPY WDECAREA                                                        
008300     EJECT                                                                
008400*    ---                                                                  
008500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008600     SKIP3                                                                
008700*01  MID -COPY W4I38501                                                   
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009000     SKIP3                                                                
009100*01  -COPY WMSGAREA                                                       
009200     EJECT                                                                
009300*    03  MOD -COPY W4O38501   -RED MSG-AREA.                              
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009600     SKIP3                                                                
009700*01  -COPY WMFSAREA                                                       
009800     EJECT                                                                
009900*    ---                                                                  
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300*                                                                         
010400 01  WS-PFI-IDTRP.                                                        
010500     03  WS-PFI-IDTRPLOS   PIC X(3)    VALUE SPACE.                       
010600     03  WS-PFI-IDTRPVAR   PIC X(2)    VALUE SPACE.                       
012700*                                                                         
012710*      --- VALID IDDC CODES                                               
012720*                                                                         
012730*01    -COPY WWDC99                                                       
012740       EJECT                                                              
012800 01  WS-PFX-IDORDER.                                                      
012900     03  WS-PFE-IDORDER    PIC 9(7).                                      
013000     03  WS-PF8-IDORDER    PIC 9(7).                                      
013100*                                                                         
013200 01  FILLER.                                                              
013300     03  WS-VKORDNTO-TOT             PIC S9(6)V9(1) COMP-3.               
013400     03  WS-VLORDNTO-TOT             PIC S9(4)V9(3) COMP-3.               
013500*                                                                         
013600 01  NYCKLAR-TILL-DLI.                                                    
013700*                                                                         
013800     03  W-4537-WDGXKEY.                                                  
013900         07 W-4537-IDHTYP        PIC X(4)    VALUE '4537'.                
014000         07 W-4537-IDDC          PIC X(02).                               
014100         07 W-4537-IDTRPLOS      PIC X(3).                                
014200         07 W-4537-LOW-VALUE     PIC X(21)   VALUE LOW-VALUE.             
014300*                                                                         
014400     03  W-4538-WDGXKEY-X.                                                
014500         07 W-4538-IDTRPVAR      PIC X(2)    VALUE SPACE.                 
014600         07 W-4538-IDORDER       PIC S9(7)   VALUE ZERO COMP-3.           
014700         07 W-4538-LOW-VALUE     PIC X(04)   VALUE LOW-VALUE.             
014800*                                                                         
014900     03  W-WDQ301KY-MIN-X.                                                
015000         07 W-Q301KY-IDORDER-MIN  PIC S9(7)   COMP-3.                     
015100         07 W-Q301KY-IDDC-MIN     PIC X(02).                              
015200         07 W-Q301KY-IDPRODNR-MIN PIC S9(7)   COMP-3.                     
015300         07 W-Q301KY-IDPLKLST-MIN PIC S9(3)   COMP-3.                     
015400*                                                                         
015500     03  W-WDQ301KY-MAX-X.                                                
015600         07 W-Q301KY-IDORDER-MAX  PIC S9(7)   COMP-3.                     
015700         07 W-Q301KY-IDDC-MAX     PIC X(02).                              
015800         07 W-Q301KY-IDPRODNR-MAX PIC S9(7)   COMP-3.                     
015900         07 W-Q301KY-IDPLKLST-MAX PIC S9(3)   COMP-3.                     
016000*                                                                         
016100 01  STATUS-WS                   PIC XX.                                  
016200     88  SEGMENT-OK                          VALUE '  '.                  
016300     88  SEGMENT-II                          VALUE 'II'.                  
016400     88  SEGMENT-GB                          VALUE 'GB'.                  
016500     88  SEGMENT-GE                          VALUE 'GE'.                  
016600     SKIP2                                                                
016700 01  GODK-STATUSKODER.                                                    
016800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016900     SKIP3                                                                
017000 01  SSA1                        PIC X(64).                               
017100 01  SSA2                        PIC X(64).                               
017200     EJECT                                                                
017300*    --- IMS FUNKTIONSKODER                                               
017400*01  -COPY W0003                                                          
017500     EJECT                                                                
017600*    ---  DLI INPUT-OUTPUT AREA                                           
017700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017800     SKIP3                                                                
017900 01  DLI-IO-AREA1.                                                        
018000     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
018100     SKIP3                                                                
018200     03  WLXXKV01 REDEFINES IO-AREA1.                                     
018300*        05  -COPY WDGX4537   -PRE XXKV-                                  
018400     EJECT                                                                
018500     03  WLXXKV11 REDEFINES IO-AREA1.                                     
018600*        05  -COPY WDGX4538   -PRE XXKV-                                  
018700     EJECT                                                                
018800 01  DLI-IO-AREA2.                                                        
018900     03  IO-AREA2                PIC X(224)  VALUE SPACE.                 
019000     SKIP3                                                                
019100     03  WLORQA01 REDEFINES IO-AREA2.                                     
019200*        05  -COPY WDQ301                                                 
019300     EJECT                                                                
019400 LINKAGE SECTION.                                                         
019500                                                                          
019600*01  -COPY W0009      -PRE MSG-                                           
019700     EJECT                                                                
019800*01  -COPY W0008      -PRE USEA-                                          
019900     05  FILLER                  PIC X.                                   
020000     EJECT                                                                
020100*01  -COPY W0008      -PRE XXKV-                                          
020200     05  FILLER                  PIC X.                                   
020300     EJECT                                                                
020400*01  -COPY W0008      -PRE ORQA-                                          
020500     05  FILLER                  PIC X.                                   
020600     EJECT                                                                
020700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB XXKV-PCB ORQA-PCB.            
020800*                                                                         
020900 W40385 SECTION.                                                          
021000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB XXKV-PCB ORQA-PCB.            
021100     PERFORM IMS-GET-MSG                                                  
021200     IF SEGMENT-OK                                                        
021300       PERFORM A-INIT                                                     
021400       PERFORM B-KOLLA-NYCKLAR                                            
021500       IF NYCKLAR-OK  AND ALLT-OK                                         
021600           IF MFS-FIRST                                                   
021700             PERFORM C-FOERSTA-SIDA                                       
021800           ELSE                                                           
021900             IF MFS-NEXT                                                  
022000               PERFORM D-NAESTA-SIDA                                      
022100             ELSE                                                         
022200               PERFORM E-SAMMA-SIDA                                       
022300             END-IF                                                       
022400           END-IF                                                         
022500           IF ALLT-OK                                                     
022600             PERFORM F-LAES-VISA-INFO                                     
022700           END-IF                                                         
022800       END-IF                                                             
022900       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O38501 + 4                      
023000       PERFORM IMS-INSERT-MSG                                             
023100     END-IF                                                               
023200     MOVE ZERO TO RETURN-CODE                                             
023300     GOBACK                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 A-INIT SECTION.                                                          
023700                                                                          
023800     IF MSG-DUBBLA-TRANSKODER                                             
023900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I38501                 
024000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
024100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024200     ELSE                                                                 
024300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I38501                  
024400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
024500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024600     END-IF                                                               
024700                                                                          
024800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
024900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
025000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025100                                                                          
025200     MOVE LOW-VALUE TO MSG-AREA                                           
025300     MOVE '4385' TO MOD-IDTRANS                                           
025400     MOVE 'W4O385N1' TO MFS-IDMOD                                         
025500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
025600                                                                          
025700     IF NOT EGEN-MID                                                      
025800       MOVE SPACE TO MFS-KDTRTYP                                          
025900       MOVE '7' TO MFS-IDPFK                                              
026000     END-IF                                                               
026100                                                                          
026200     .                                                                    
026300     EJECT                                                                
026400 B-KOLLA-NYCKLAR SECTION.                                                 
026500                                                                          
026600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026700     MOVE '001'             TO MSGI-KDCALL                                
026800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026900     MOVE '4385'            TO MSGI-IDTRANS                               
027000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
027100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027200     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
027300     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
027400                                                                          
027500     MOVE JA  TO NYCKLAR-SW                                               
027600     MOVE '0' TO TRP-SW                                                   
027700                                                                          
027800     MOVE MFS-RENSA-FAELT TO MOD-PFI-IDTRP                                
027900                             MOD-PFI-IDDC                                 
028000                                                                          
028100     IF MID-PFI-IDTRP = ALL '+'                                           
028200       MOVE MID-PF7-IDTRP TO WS-PFI-IDTRP                                 
028300     ELSE                                                                 
028400       MOVE MID-PFI-IDTRP TO WS-PFI-IDTRP                                 
028500       MOVE '7'             TO MFS-IDPFK                                  
028600       MOVE SPACE           TO MFS-KDTRTYP                                
028700     END-IF                                                               
028800                                                                          
028900     IF WS-PFI-IDTRPLOS NOT NUMERIC                                       
029000        MOVE NEJ TO NYCKLAR-SW                                            
029100     ELSE                                                                 
029200        MOVE '3'   TO TRP-SW                                              
029300     END-IF                                                               
029400                                                                          
029500     IF WS-PFI-IDTRPVAR NOT NUMERIC                                       
029600        MOVE SPACE TO WS-PFI-IDTRPVAR                                     
029700     ELSE                                                                 
029800        MOVE '5'   TO TRP-SW                                              
029900     END-IF                                                               
030000                                                                          
030100     MOVE MSGI-IDDC               TO WS-IDDC                              
030200                                                                          
030300     MOVE WS-IDDC            TO MOD-PF7-IDDC                              
030400                                                                          
030500     IF GODK-MID OR NYCKLAR-OK                                            
030600        MOVE WS-PFI-IDTRP    TO MOD-PF7-IDTRP                             
030700     ELSE                                                                 
030800        MOVE MFS-RENSA-FAELT TO MOD-PF7-IDTRP                             
030900     END-IF                                                               
031000                                                                          
031100     IF NYCKLAR-FEL                                                       
031200        MOVE '401'      TO MED-IDMFSFEL                                   
031300        CALL WMEDKONV USING MED-WMEDAREA                                  
031400        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
031500        PERFORM MFS-RENSA-FAELT-IN                                        
031600        PERFORM MFS-RENSA-FAELT-UT                                        
031700     ELSE                                                                 
031800        MOVE WS-IDDC         TO W-4537-IDDC                               
031900        MOVE WS-PFI-IDTRPLOS TO W-4537-IDTRPLOS                           
032000        MOVE WS-PFI-IDTRPVAR TO W-4538-IDTRPVAR                           
032100        PERFORM IMS-GU-XXKV-WDGX4537                                      
032200        IF SEGMENT-GE                                                     
032300           MOVE NEJ        TO ALLT-SW                                     
032400           MOVE '413'      TO MED-IDMFSFEL                                
032500           CALL WMEDKONV USING MED-WMEDAREA                               
032600           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
032700           PERFORM MFS-RENSA-FAELT-IN                                     
032800           PERFORM MFS-RENSA-FAELT-UT                                     
032900        END-IF                                                            
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 C-FOERSTA-SIDA SECTION.                                                  
033400                                                                          
033500     MOVE '006' TO MED-IDMFSFEL                                           
033600     CALL WMEDKONV USING MED-WMEDAREA                                     
033700     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
033800     PERFORM MFS-RENSA-FAELT-IN                                           
033900     PERFORM MFS-RENSA-FAELT-UT                                           
034000     .                                                                    
034100     EJECT                                                                
034200 D-NAESTA-SIDA SECTION.                                                   
034300                                                                          
034400     IF MID-PF8-IDTRP = 'SLUT'                                            
034500        MOVE '115'           TO MED-IDMFSFEL                              
034600        CALL WMEDKONV USING MED-WMEDAREA                                  
034700        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
034800        PERFORM MFS-RENSA-FAELT-IN                                        
034900        PERFORM MFS-RENSA-FAELT-UT                                        
035000        MOVE 'SLUT'          TO MOD-PF8-IDTRP                             
035100        MOVE NEJ             TO ALLT-SW                                   
035200     ELSE                                                                 
035300        MOVE MID-PF8-IDTRP   TO  WS-PFI-IDTRP                             
035400        MOVE WS-PFI-IDTRPVAR TO  W-4538-IDTRPVAR                          
035500        MOVE MID-PF8-IDORDER TO  WS-PF8-IDORDER                           
035600        MOVE WS-PF8-IDORDER  TO  W-4538-IDORDER                           
035700     END-IF                                                               
035800     .                                                                    
035900     EJECT                                                                
036000                                                                          
036100 E-SAMMA-SIDA SECTION.                                                    
036200                                                                          
036300     MOVE MID-PFE-IDTRP   TO  WS-PFI-IDTRP                                
036400     MOVE WS-PFI-IDTRPVAR TO  W-4538-IDTRPVAR                             
036500     MOVE MID-PFE-IDORDER TO  WS-PFE-IDORDER                              
036600     MOVE WS-PFE-IDORDER  TO  W-4538-IDORDER                              
036700     .                                                                    
036800     EJECT                                                                
036900 F-LAES-VISA-INFO SECTION.                                                
037000                                                                          
037100     MOVE 0               TO WS-VLORDNTO-TOT                              
037200                             WS-VKORDNTO-TOT                              
037300                                                                          
037400     IF MFS-FIRST                                                         
037500        IF TRP-3                                                          
037600           PERFORM IMS-GNP-XXKV-WDGX4538-OKVAL                            
037700        ELSE                                                              
037800           MOVE NEJ TO DELKVAL-SW                                         
037900           PERFORM UNTIL DELKVAL-OK                                       
038000              PERFORM IMS-GNP-XXKV-WDGX4538-OKVAL                         
038100              IF SEGMENT-OK                                               
038200                 IF XXKV-4538-IDTRPVAR = WS-PFI-IDTRPVAR                  
038300                    MOVE JA TO DELKVAL-SW                                 
038400                 END-IF                                                   
038500              ELSE                                                        
038600                 MOVE JA TO DELKVAL-SW                                    
038700              END-IF                                                      
038800           END-PERFORM                                                    
038900        END-IF                                                            
039000     ELSE                                                                 
039100        PERFORM IMS-GNP-XXKV-WDGX4538-KVAL                                
039200     END-IF                                                               
039300                                                                          
039400     IF SEGMENT-GE                                                        
039500        MOVE '413' TO MED-IDMFSFEL                                        
039600        CALL WMEDKONV USING MED-WMEDAREA                                  
039700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
039800        PERFORM MFS-RENSA-FAELT-UT                                        
039900      ELSE                                                                
040000        MOVE +1 TO INDX                                                   
040100        MOVE WS-PFI-IDTRP       TO MOD-PFE-IDTRP                          
040200        MOVE XXKV-4538-IDTRPVAR TO MOD-IDTRPVAR IN MOD-PFE-IDTRP          
040300        MOVE XXKV-4538-IDORDER  TO MOD-PFE-IDORDER                        
040400                                                                          
040500        PERFORM UNTIL INDX > MAX-INDX                                     
040600          IF SEGMENT-OK                                                   
040700             MOVE XXKV-4538-IDTRPVAR TO WS-PFI-IDTRPVAR                   
040800             MOVE XXKV-4538-IDDISTR  TO MOD-IDDISTR-RAD (INDX)            
040900             MOVE XXKV-4538-IDKUNDNR TO MOD-IDKUNDNR-RAD (INDX)           
041000             MOVE XXKV-4538-IDORDNR7 TO MOD-IDORDER-RAD (INDX)            
041100*                                                                         
042100             MOVE XXKV-4538-TIRFS    TO MOD-TIRFS-RAD (INDX)              
042300*                                                                         
042510             MOVE XXKV-4538-VKORDNTO TO WS-VLORDNTO                       
042520             MOVE XXKV-4538-VLORDNTO TO WS-VKORDNTO                       
042530             IF US-MEASUREMENT                                            
042540               COMPUTE WS-VLORDNTO ROUNDED =                              
042550                       WS-VLORDNTO * CONV-M3-TO-FT3 END-COMPUTE           
042560               COMPUTE WS-VKORDNTO ROUNDED =                              
042570                       WS-VKORDNTO * CONV-KG-TO-LB  END-COMPUTE           
042580             END-IF                                                       
042590             MOVE WS-VLORDNTO        TO MOD-VLORDNTO-RAD (INDX)           
042591             MOVE WS-VKORDNTO        TO MOD-VKORDNTO-RAD (INDX)           
042592*                                                                         
042600             PERFORM FA-CHECK-STATUS                                      
042700             MOVE W-KDORDSTA         TO MOD-KDORDSTA-RAD (INDX)           
042800             ADD  XXKV-4538-VLORDNTO TO WS-VLORDNTO-TOT                   
042900             ADD  XXKV-4538-VKORDNTO TO WS-VKORDNTO-TOT                   
043000             IF TRP-3                                                     
043100                PERFORM IMS-GNP-XXKV-WDGX4538-OKVAL                       
043200             ELSE                                                         
043300                MOVE NEJ TO DELKVAL-SW                                    
043400                PERFORM UNTIL DELKVAL-OK                                  
043500                   PERFORM IMS-GNP-XXKV-WDGX4538-OKVAL                    
043600                   IF SEGMENT-OK                                          
043700                      IF XXKV-4538-IDTRPVAR = WS-PFI-IDTRPVAR             
043800                         MOVE JA TO DELKVAL-SW                            
043900                      END-IF                                              
044000                   ELSE                                                   
044100                      MOVE JA TO DELKVAL-SW                               
044200                   END-IF                                                 
044300                END-PERFORM                                               
044400             END-IF                                                       
044500          ELSE                                                            
044600             PERFORM MFS-RENSA-RAD                                        
044700          END-IF                                                          
044800          ADD 1 TO INDX                                                   
044900       END-PERFORM                                                        
045000                                                                          
045100       IF SEGMENT-OK                                                      
045200          MOVE '105' TO MED-IDMFSINF                                      
045300          CALL WMEDKONV USING MED-WMEDAREA                                
045400          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
045500          MOVE WS-PFI-IDTRP       TO MOD-PF8-IDTRP                        
045600          MOVE XXKV-4538-IDTRPVAR TO MOD-IDTRPVAR IN MOD-PF8-IDTRP        
045700          MOVE XXKV-4538-IDORDER  TO MOD-PF8-IDORDER                      
045800       ELSE                                                               
045900          MOVE '106' TO MED-IDMFSINF                                      
046000          CALL WMEDKONV USING MED-WMEDAREA                                
046100          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
046200          MOVE MFS-RENSA-FAELT    TO MOD-TEMFSFEL                         
046300          MOVE 'SLUT'             TO MOD-PF8-IDTRP                        
046400          MOVE 9999999            TO MOD-PF8-IDORDER                      
046500       END-IF                                                             
046600       IF MFS-FIRST                                                       
046700          PERFORM UNTIL NOT SEGMENT-OK                                    
046800             ADD  XXKV-4538-VLORDNTO TO WS-VLORDNTO-TOT                   
046900             ADD  XXKV-4538-VKORDNTO TO WS-VKORDNTO-TOT                   
047000             IF TRP-3                                                     
047100                PERFORM IMS-GNP-XXKV-WDGX4538-OKVAL                       
047200             ELSE                                                         
047300                MOVE NEJ TO DELKVAL-SW                                    
047400                PERFORM UNTIL DELKVAL-OK                                  
047500                   PERFORM IMS-GNP-XXKV-WDGX4538-OKVAL                    
047600                   IF SEGMENT-OK                                          
047700                      IF XXKV-4538-IDTRPVAR = WS-PFI-IDTRPVAR             
047800                         MOVE JA TO DELKVAL-SW                            
047900                      END-IF                                              
048000                   ELSE                                                   
048100                      MOVE JA TO DELKVAL-SW                               
048200                   END-IF                                                 
048300                END-PERFORM                                               
048400             END-IF                                                       
048500          END-PERFORM                                                     
048600       END-IF                                                             
048700       IF MFS-FIRST                                                       
048710*                                                                         
048800          MOVE WS-VLORDNTO-TOT   TO  WS-VLORDNTO                          
048900          MOVE WS-VKORDNTO-TOT   TO  WS-VKORDNTO                          
048940          IF US-MEASUREMENT                                               
048950            COMPUTE WS-VLORDNTO ROUNDED =                                 
048960                    WS-VLORDNTO * CONV-M3-TO-FT3 END-COMPUTE              
048970            COMPUTE WS-VKORDNTO ROUNDED =                                 
048980                    WS-VKORDNTO * CONV-KG-TO-LB  END-COMPUTE              
048990          END-IF                                                          
048991          MOVE WS-VLORDNTO       TO MOD-VLORDNTO-TOT                      
048992          MOVE WS-VKORDNTO       TO MOD-VKORDNTO-TOT                      
048993*                                                                         
049000       ELSE                                                               
049100          PERFORM FB-EDIT-MID                                             
049200       END-IF                                                             
049300     END-IF                                                               
049400     .                                                                    
049500     EJECT                                                                
049600 FA-CHECK-STATUS   SECTION.                                               
049700                                                                          
049800     MOVE NEJ                  TO KDORDSTA-SW                             
049900     MOVE SPACE                TO W-KDORDSTA                              
050000     MOVE HIGH-VALUE           TO W-WDQ301KY-MAX-X                        
050100     MOVE LOW-VALUE            TO W-WDQ301KY-MIN-X                        
050200                                                                          
050300     MOVE XXKV-4538-IDORDER    TO W-Q301KY-IDORDER-MIN                    
050400                                  W-Q301KY-IDORDER-MAX                    
050500     MOVE WS-IDDC              TO W-Q301KY-IDDC-MIN                       
050600                                  W-Q301KY-IDDC-MAX                       
050700                                                                          
050800     PERFORM IMS-GU-ORQA-WDQ301                                           
050900     PERFORM UNTIL SEGMENT-GB OR SEGMENT-GE OR KDORDSTA-OK                
051000                                                                          
051100         EVALUATE ODEL-KDODELSTA                                          
051200         WHEN 'R'                                                         
051300            IF W-KDORDSTA      =  SPACE                                   
051400                MOVE 'R '      TO W-KDORDSTA                              
051500             ELSE                                                         
051600                IF W-KDORDSTA (1:1) = 'U' OR 'P'                          
051700                    MOVE 'R*'       TO W-KDORDSTA                         
051800                    MOVE JA         TO KDORDSTA-SW                        
051900                END-IF                                                    
052000            END-IF                                                        
052100                                                                          
052200         WHEN 'U'                                                         
052300            EVALUATE W-KDORDSTA                                           
052400              WHEN SPACE                                                  
052500                 MOVE 'U '     TO W-KDORDSTA                              
052600              WHEN 'R '                                                   
052700                 MOVE 'R*'     TO W-KDORDSTA                              
052800                 MOVE JA       TO KDORDSTA-SW                             
052900              WHEN 'P '                                                   
053000                 MOVE 'U*'     TO W-KDORDSTA                              
053100            END-EVALUATE                                                  
053200                                                                          
053300         WHEN 'P'                                                         
053400            EVALUATE W-KDORDSTA                                           
053500              WHEN SPACE                                                  
053600                 MOVE 'P '       TO W-KDORDSTA                            
053700              WHEN 'R '                                                   
053800                    MOVE 'R*'    TO W-KDORDSTA                            
053900                    MOVE JA      TO KDORDSTA-SW                           
054000              WHEN 'U '                                                   
054100                       MOVE 'U*' TO W-KDORDSTA                            
054200            END-EVALUATE                                                  
054300                                                                          
054400         END-EVALUATE                                                     
054500         PERFORM IMS-GN-ORQA-WDQ301                                       
054600     END-PERFORM                                                          
054700     .                                                                    
054800     EJECT                                                                
054900 FB-EDIT-MID SECTION.                                                     
055000                                                                          
055100     MOVE MID-TOT-VLORDNTO TO DEC-IDFRIDATA                               
055200     MOVE 4 TO DEC-KVHELTAL                                               
055300     MOVE 3 TO DEC-KVDECIMAL                                              
055400     CALL WDECEDIT USING DEC-WDECAREA                                     
055500     IF DEC-KDSVAR-FEL                                                    
055600         MOVE MFS-ALFA-FAELT-FEL TO MOD-VLORDNTO-TOT-ATTR                 
055700     ELSE                                                                 
055810         MOVE DEC-IDEDITDATA     TO WS-VLORDNTO                           
055830           IF US-MEASUREMENT                                              
055840             COMPUTE WS-VLORDNTO ROUNDED =                                
055850                     WS-VLORDNTO * CONV-M3-TO-FT3 END-COMPUTE             
055880           END-IF                                                         
055890         MOVE WS-VLORDNTO        TO MOD-VLORDNTO-TOT                      
055900     END-IF                                                               
055910                                                                          
056000     MOVE MID-TOT-VKORDNTO TO DEC-IDFRIDATA                               
056100     MOVE 6 TO DEC-KVHELTAL                                               
056200     MOVE 1 TO DEC-KVDECIMAL                                              
056300     CALL WDECEDIT USING DEC-WDECAREA                                     
056400     IF DEC-KDSVAR-FEL                                                    
056500         MOVE MFS-ALFA-FAELT-FEL TO MOD-VKORDNTO-TOT-ATTR                 
056600     ELSE                                                                 
056710         MOVE DEC-IDEDITDATA     TO WS-VKORDNTO                           
056720           IF US-MEASUREMENT                                              
056730             COMPUTE WS-VKORDNTO ROUNDED =                                
056740                     WS-VKORDNTO * CONV-KG-TO-LB END-COMPUTE              
056750           END-IF                                                         
056760         MOVE WS-VKORDNTO        TO MOD-VKORDNTO-TOT                      
056800     END-IF                                                               
056900     .                                                                    
057700     SKIP2                                                                
057800 MFS-RENSA-FAELT-UT SECTION.                                              
057900*                                                                         
058000     MOVE +1 TO INDX                                                      
058100     PERFORM UNTIL INDX > MAX-INDX                                        
058200        PERFORM MFS-RENSA-RAD                                             
058300           ADD +1 TO INDX                                                 
058400     END-PERFORM                                                          
058500*                                                                         
058600     MOVE MFS-RENSA-FAELT  TO  MOD-PFE-IDTRP                              
058700                               MOD-PF8-IDTRP                              
058800     .                                                                    
058900     SKIP2                                                                
059000 MFS-RENSA-RAD SECTION.                                                   
059100           MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR-RAD (INDX)              
059200                                      MOD-IDKUNDNR-RAD (INDX)             
059300                                      MOD-IDORDER-RAD (INDX)              
059400                                      MOD-TIRFS-RAD (INDX)                
059500           MOVE MFS-RENSA-FAELT    TO MOD-VKORDNTO-RAD (INDX)             
059600                                      MOD-VLORDNTO-RAD (INDX)             
059700                                      MOD-KDORDSTA-RAD (INDX)             
059800       .                                                                  
059900*                                                                         
060000 MFS-RENSA-FAELT-IN SECTION.                                              
060100*                                                                         
060200     MOVE MFS-RENSA-FAELT TO MOD-PFI-IDTRP                                
060300                             MOD-PFI-IDDC                                 
060400     .                                                                    
062500     SKIP2                                                                
063200     EJECT                                                                
063300                                                                          
063400 IMS-GET-MSG SECTION.                                                     
063500                                                                          
063600     MOVE '  QC' TO GODK-STATUSKODER                                      
063700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
063800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     .                                                                    
064100     SKIP3                                                                
064200                                                                          
064300 IMS-INSERT-MSG SECTION.                                                  
064400                                                                          
064500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
064600       MOVE '0' TO MFS-KDHUVOMR                                           
064700     END-IF                                                               
064800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
064900     MOVE SPACE TO GODK-STATUSKODER                                       
065000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
065100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065200     PERFORM IMS-STATUSKONTROLL                                           
065300     .                                                                    
065400     EJECT                                                                
065500                                                                          
065600 IMS-GU-ORQA-WDQ301  SECTION.                                             
065700                                                                          
065800     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
065900                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
066000          DELIMITED BY SIZE INTO SSA1                                     
066100     MOVE '  GE' TO GODK-STATUSKODER                                      
066200     CALL CBLTDLI USING GU   ORQA-PCB DLI-IO-AREA2 SSA1                   
066300     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
066400     PERFORM IMS-STATUSKONTROLL                                           
066500     .                                                                    
066600                                                                          
066700 IMS-GN-ORQA-WDQ301  SECTION.                                             
066800                                                                          
066900     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
067000                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
067100          DELIMITED BY SIZE INTO SSA1                                     
067200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
067300     CALL CBLTDLI USING GN   ORQA-PCB DLI-IO-AREA2 SSA1                   
067400     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
067500     PERFORM IMS-STATUSKONTROLL                                           
067600     .                                                                    
067700                                                                          
067800 IMS-GNP-XXKV-WDGX4538-OKVAL SECTION.                                     
067900                                                                          
068000     MOVE 'WLXXKV11'           TO SSA1                                    
068100     MOVE '  GE' TO GODK-STATUSKODER                                      
068200     CALL CBLTDLI USING GNP XXKV-PCB DLI-IO-AREA1 SSA1                    
068300     MOVE XXKV-STATUS-CODE TO STATUS-WS                                   
068400     PERFORM IMS-STATUSKONTROLL                                           
068500     .                                                                    
068600                                                                          
068700 IMS-GNP-XXKV-WDGX4538-KVAL  SECTION.                                     
068800                                                                          
068900     STRING 'WLXXKV11(WDGXKEY  =' W-4538-WDGXKEY-X ')'                    
069000          DELIMITED BY SIZE INTO SSA1                                     
069100     MOVE '  GE' TO GODK-STATUSKODER                                      
069200     CALL CBLTDLI USING GNP XXKV-PCB DLI-IO-AREA1 SSA1                    
069300     MOVE XXKV-STATUS-CODE TO STATUS-WS                                   
069400     PERFORM IMS-STATUSKONTROLL                                           
069500     .                                                                    
069600                                                                          
069700                                                                          
069800 IMS-GU-XXKV-WDGX4537 SECTION.                                            
069900                                                                          
070000     STRING 'WLXXKV01(WDGXKEY  =' W-4537-WDGXKEY ')'                      
070100          DELIMITED BY SIZE INTO SSA1                                     
070200     MOVE '  GE' TO GODK-STATUSKODER                                      
070300     CALL CBLTDLI USING GU  XXKV-PCB DLI-IO-AREA1 SSA1                    
070400     MOVE XXKV-STATUS-CODE TO STATUS-WS                                   
070500     PERFORM IMS-STATUSKONTROLL                                           
070600     .                                                                    
070700                                                                          
070800 IMS-STATUSKONTROLL SECTION.                                              
070900                                                                          
071000     SET STATUS-IX TO 1                                                   
071100     SEARCH GODK-STATUS                                                   
071200       AT END CALL FELLOG                                                 
071300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
071400     END-SEARCH                                                           
071500     .                                                                    
