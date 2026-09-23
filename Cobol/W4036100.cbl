000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4036100.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   JAN.  90.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        UPPDATERING,NYUPPLÄGGNING,BORTTAG OCH FRÅGEPROGRAM               
001100*                                                                         
001200*        UNDERHÅLL AV PRODUKTIONSKLASSTABELL.                             
001300*                                                                         
001400*        PGM:ET VISAR OCH GÖR FÖRÄNDRINGAR MOT DEN TABELL SOM             
001500*        VISAR HUR DEN PROD.KLASS.TABELL SER UT SOM AVGÖR VILKEN          
001600*        PKLASS ORDERN FÅR.                                               
001700*                                                                         
001800*        DE FAKTORER SOM PÅVERKAR VALET AV PKLASS ÄR :                    
001900*        ORDERKLASS                                                       
002000*        VIKT                                                             
002100*        VOLYM                                                            
002200*        ANTAL RADER.                                                     
002300*                                                                         
002400*        VID NYUPPLÄGG AV PKLASSTAB. HÄMTAS ALLTID EN DEFAULT-            
002500*        TABELL SOM INNEHÅLLER KLASS OCH HLO-TABELL.                      
002600*                                                                         
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W4T361                                              
003000*        MID:         W4I36101                                            
003100*        FORMAT:      W4F361                                              
003200*        DATABAS:     WDR1                                                
003300*                                                                         
003400*    UTDATA.                                                              
003500*        MOD:         W4O36101                                            
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900 DATA DIVISION.                                                           
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                   PIC X(8)    VALUE 'W4036100'.                
004300 77  JA                      PIC X       VALUE 'J'.                       
004400 77  NEJ                     PIC X       VALUE 'N'.                       
004500 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
004600 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
004700 77  BAS-INDX                PIC S9(9)   VALUE +0   COMP SYNC.            
004800 77  MAX-RADER               PIC S9(9)   VALUE +13  COMP SYNC.            
004900 77  MAX-RADER-I-BASTABELL   PIC S9(9)   VALUE +6   COMP SYNC.            
005000*** VID ÄNDRING AV ORDERKLASS UPPDATERA ÄVEN MAX-RADER-I-BASTABELL        
005100 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +723 COMP SYNC.            
005200 77  IDPKLTAB-WS             PIC X(2)    VALUE SPACE.                     
005300 77  VKORDNTO-X              PIC  9(6).9(1) VALUE ZERO.                   
005400 77  VLORDNTO-X              PIC  9(4).9(3) VALUE ZERO.                   
005500 77  VLORDNTO-X-SIGN         PIC S9(4)V9(3) VALUE ZERO.                   
005600 77  VKORDNTO-X-SIGN         PIC S9(6)V9(1) VALUE ZERO.                   
005700 77  VLORDNTO-KOLL           PIC S9(4)V9(3) VALUE 9999.999 COMP-3.        
005800 77  VKORDNTO-KOLL           PIC S9(6)V9(1) VALUE 999999.9 COMP-3.        
005900 77  KVRADER-KOLL            PIC S9(5)      VALUE 99999 COMP-3.           
005901 77  WSS-KDORDKL             PIC  9(1).                                   
005902*                                                                         
005903 77  WS-IDRADNR              PIC  9(5).                                   
005904*                                                                         
006100 77  INDATA-SW               PIC X       VALUE 'J'.                       
006200   88  INDATA-OK                         VALUE 'J'.                       
006300   88  INDATA-FEL                        VALUE 'N'.                       
006400                                                                          
006500 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
006600   88  NYCKLAR-OK                        VALUE 'J'.                       
006700   88  NYCKLAR-FEL                       VALUE 'N'.                       
006800                                                                          
006900 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
007000   88  EGEN-TRANS                        VALUE '4361'.                    
007100   88  GODK-TRANS                        VALUE '4361' '4362'              
007200                                               '4363' '4366'.             
007300                                                                          
007400 77  RADNUMMER-SW            PIC X       VALUE 'J'.                       
007500   88  RADNUMMER-FINNS                   VALUE 'J'.                       
007600                                                                          
007700 77  AENDRING-SW             PIC X       VALUE 'J'.                       
007800   88  AENDRING                          VALUE 'J'.                       
007900                                                                          
008000 77  ALLT-SW                 PIC X       VALUE 'J'.                       
008100   88  ALLT-OK                           VALUE 'J'.                       
008200                                                                          
008300 77  BASTAB                  PIC X(2)    VALUE SPACE.                     
008400    88  BASTABELL                        VALUE ' 1' '01'.                 
008500                                                                          
008600 77  DUBLETT-RAD             PIC X(5)    VALUE SPACE.                     
008700   88  RADNUMMER-FEL                     VALUE '00100'                    
008800                                               '00200' '00300'            
008900                                               '00400' '00500'.           
009000                                                                          
009100 01  TABELL.                                                              
009200     03 HELP-TABELL OCCURS 13.                                            
009300        05  TAB-IDRADNR      PIC S9(5)                 COMP-3.            
009400        05  TAB-LOW-VALUE    PIC X(2).                                    
009500        05  TAB-IDHLOTAB     PIC 9(2).                                    
009600        05  TAB-KDORDKL      PIC S9                    COMP-3.            
009700        05  TAB-KDPRODKL     PIC X.                                       
009800        05  TAB-KVRADER      PIC S9(5)                 COMP-3.            
009900        05  TAB-VKORDNTO     PIC S9(6)V9(1)            COMP-3.            
010000        05  TAB-VLORDNTO     PIC S9(4)V9(3)            COMP-3.            
010100                                                                          
010200     EJECT                                                                
010300                                                                          
010400 01  NOLLMASK.                                                            
010500                                                                          
010600        05  FILLER           PIC S9(5)      VALUE ZERO COMP-3.            
010700        05  FILLER           PIC X(2)       VALUE LOW-VALUE.              
010800        05  FILLER           PIC 9(2)       VALUE ZERO.                   
010900        05  FILLER           PIC S9         VALUE ZERO COMP-3.            
011000        05  FILLER           PIC X          VALUE SPACE.                  
011100        05  FILLER           PIC S9(5)      VALUE 99999 COMP-3.           
011200        05  FILLER           PIC S9(6)V9(1) VALUE 999999.9 COMP-3.        
011300        05  FILLER           PIC S9(4)V9(3) VALUE 9999.999 COMP-3.        
011400     EJECT                                                                
011500                                                                          
011600 01  GENERELLA-SUBPROGRAM.                                                
011700   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
011800   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
011900   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
012000   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
012010   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
012100     EJECT                                                                
012200*   -COPY WDECAREA                                                        
012400     EJECT                                                                
012500*   -COPY WMEDAREA                                                        
012700     EJECT                                                                
012710*                   ****    PARAMETRAR TILL W005INIT                      
012720*01  -COPY WMSGINIT                                                       
012730     EJECT                                                                
012800******************************************************************        
012900*                                                                         
013000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
013100*                                                                         
013200 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
013300     SKIP3                                                                
013400*01  MID -COPY W4I36101 -PRE MID-                                         
013600     EJECT                                                                
013700*01  -COPY WMSGAREA                                                       
013900     EJECT                                                                
014000*  03  MOD -COPY W4O36101 -RED MSG-AREA -PRE MOD-                         
014200     EJECT                                                                
014300*01  -COPY WMFSAREA                                                       
014500     EJECT                                                                
014600******************************************************************        
014700*                                                                         
014800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014900*                                                                         
015000 01  IMS-WS.                                                              
015100   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
015200     SKIP3                                                                
015300*                        **** STATUS-KOD FRÅN IMS                         
015400   03  STATUS-WS             PIC XX.                                      
015500     88  SEGMENT-FINNS                   VALUE '  '.                      
015600     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
015700     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
015800     88  SEGMENT-HOGRE                   VALUE 'GA'.                      
015900     SKIP3                                                                
016000   03  GODK-STATUSKODER.                                                  
016100     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200     SKIP3                                                                
016300 01  NYCKLAR-TILL-DLI.                                                    
016400   03  W-IDHTYP-X.                                                        
016500     05  W-IDHTYP            PIC X(4)   VALUE '4443'.                     
016600     05  W-IDDC              PIC X(2)   VALUE '00'.                       
016700     05  W-IDPKLTAB          PIC X(2)   VALUE SPACE.                      
016800     05  4443-LOW-VALUE      PIC X(22)  VALUE LOW-VALUE.                  
016900                                                                          
017000   03  W-IDHTYP-X-BAS.                                                    
017100     05  W-IDHTYP-BAS        PIC X(4)   VALUE '4443'.                     
017200     05  W-IDDC-BAS          PIC X(2)   VALUE '00'.                       
017300     05  W-IDPKLTAB-BAS      PIC X(2)   VALUE '01'.                       
017400     05  4443-LOW-VALUE-BAS  PIC X(22)  VALUE LOW-VALUE.                  
017500                                                                          
017600   03  W-IDRADNR-X.                                                       
017700     05  W-IDRADNR           PIC S9(5)   VALUE ZERO  COMP-3.              
017800     05  4444-LOW-VALUE      PIC X(2)    VALUE LOW-VALUE.                 
017900                                                                          
018000   03  W-IDHLOTAB-X.                                                      
018100     05  W-IDDC-HLO          PIC X(2)    VALUE '00'.                      
018200     05  W-IDHLOTAB          PIC 9(2)    VALUE ZERO.                      
018300     05  4442-LOW-VALUE      PIC X(6)    VALUE LOW-VALUE.                 
018400                                                                          
018500                                                                          
018600   03  W-IDHLOTAB-ROT.                                                    
018700     05  W-HLOHTYP           PIC X(4)    VALUE '4441'.                    
018800     05  4441-LOW-VALUE      PIC X(26)   VALUE LOW-VALUE.                 
018900                                                                          
018910   03  W-IDDC-B6-X.                                                       
018920       05 W-IDDC-B6                  PIC X(2).                            
018940                                                                          
019000     SKIP3                                                                
019100 01    SSA1                  PIC X(64).                                   
019200 01    SSA2                  PIC X(64).                                   
019300     EJECT                                                                
019400*                            IMS FUNKTIONSKODER                           
019500*01    -COPY W0003                                                        
019700     EJECT                                                                
019800*                            DLI INPUT-OUTPUT AREA                        
019900 01  DLI-IO-AREA.                                                         
020000   03  IO-AREA               PIC X(300)  VALUE SPACE.                     
020100     SKIP3                                                                
020200*  03  WLXXKE01  -COPY WDGX4442    -PRE XXKE-  -RED IO-AREA               
020400     EJECT                                                                
020500*  03  WLXXKF01  -COPY WDGX4443    -PRE XXKF-  -RED IO-AREA               
020700     EJECT                                                                
020800*  03  WLXXKF11  -COPY WDGX4444    -PRE XXKF-  -RED IO-AREA               
020900                                                                          
020910 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
020920 01   DLI-IO-AREA-B601.                                                   
020930*     03  -COPY WDB601                                                    
020940                                                                          
021000     EJECT                                                                
021100 LINKAGE SECTION.                                                         
021200*01  -COPY W0009     -PRE MSG-                                            
021400     EJECT                                                                
021420*01  -COPY W0008     -PRE USEA-                                           
021430     05  FILLER              PIC X.                                       
021440     EJECT                                                                
021500*01  -COPY W0008     -PRE XXKF-                                           
021700     05  FILLER              PIC X.                                       
021800     EJECT                                                                
021900*01  -COPY W0008     -PRE XXKE-                                           
022100     05  FILLER              PIC X.                                       
022200     EJECT                                                                
022210*01  -COPY W0008     -PRE WDB6-                                           
022220     05  FILLER              PIC X.                                       
022230     EJECT                                                                
022300 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
022310                                  XXKF-PCB XXKE-PCB WDB6-PCB.             
022400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
022410                                   XXKF-PCB XXKE-PCB WDB6-PCB.            
022500                                                                          
022600     PERFORM IMS-GET-MSG                                                  
022700     IF SEGMENT-FINNS                                                     
022800       PERFORM A-INIT                                                     
022900       PERFORM B-KOLLA-NYCKLAR                                            
023000       IF NYCKLAR-OK                                                      
023100         PERFORM IMS-GET-XXKF-TABELL                                      
023200           IF MFS-UPDATE                                                  
023300             PERFORM G-KOLLA-INPUT                                        
023400             IF INDATA-OK                                                 
023500               PERFORM H-UPPDATERA                                        
023600             END-IF                                                       
023700           ELSE                                                           
023800             IF SEGMENT-FINNS                                             
023900                IF MFS-FIRST                                              
024000                  MOVE JA TO INDATA-SW                                    
024100                  MOVE ZERO TO W-IDRADNR                                  
024200                  MOVE '006' TO MED-IDMFSFEL                              
024300***************   DETTA ÄR FÖRSTA SIDAN *********************             
024400                  CALL WMEDKONV USING MED-WMEDAREA                        
024500                  MOVE MED-MFSFEL TO MOD-TEMFSFEL                         
024600                ELSE                                                      
024700                  IF MFS-NEXT                                             
024800                    MOVE JA TO INDATA-SW                                  
024900                    MOVE MID-IDRADNR-DOLD-X TO W-IDRADNR                  
025000                  ELSE                                                    
025100                    MOVE JA TO INDATA-SW                                  
025200                    PERFORM E-SAMMA-SIDA                                  
025300                  END-IF                                                  
025400                END-IF                                                    
025500             ELSE                                                         
025600                MOVE '023' TO MED-IDMFSFEL                                
025700*************** TABELL SAKNAS I BASEN *********************               
025800                CALL WMEDKONV USING MED-WMEDAREA                          
025900                MOVE MED-MFSFEL TO MOD-TEMFSFEL                           
026000                PERFORM MFS-RENSA-FAELT-IN                                
026100                PERFORM MFS-RENSA-FAELT-UT                                
026200                MOVE NEJ TO NYCKLAR-SW                                    
026300             END-IF                                                       
026400           END-IF                                                         
026500           IF NYCKLAR-OK AND INDATA-OK                                    
026600             PERFORM F-LAES-VISA-XXKF-RADER                               
026700           END-IF                                                         
026800       END-IF                                                             
026900       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
027000       PERFORM IMS-INSERT-MSG                                             
027100     END-IF                                                               
027200                                                                          
027300     MOVE ZERO TO RETURN-CODE                                             
027400     GOBACK                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 A-INIT SECTION.                                                          
027800                                                                          
027900     IF MSG-DUBBLA-TRANSKODER                                             
028000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I36101                 
028100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
028200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
028300     ELSE                                                                 
028400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I36101                  
028500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
028600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028700     END-IF                                                               
028800                                                                          
028900     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
029000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
029100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
029200                                                                          
029300     MOVE LOW-VALUE TO MSG-AREA                                           
029400     MOVE 'W4O361N1' TO MFS-IDMOD                                         
029500     MOVE '4361' TO MOD-IDTRANS                                           
029600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
029700                                                                          
029800     MOVE +1 TO INDX                                                      
029900     PERFORM UNTIL INDX > MAX-RADER                                       
030000        MOVE NOLLMASK TO HELP-TABELL(INDX)                                
030100        ADD +1 TO INDX                                                    
030200     END-PERFORM                                                          
030300                                                                          
030400                                                                          
030500     IF NOT EGEN-TRANS                                                    
030600       MOVE SPACE TO MFS-KDTRTYP                                          
030700       MOVE '7' TO MFS-IDPFK                                              
030800     END-IF                                                               
030900                                                                          
031000     IF MFS-UPDATE AND MID-IDPKLTAB-IN NOT = ALL '+'                      
031100       MOVE SPACE TO MFS-KDTRTYP                                          
031200       MOVE '7' TO MFS-IDPFK                                              
031300     END-IF                                                               
031400                                                                          
032800     MOVE SPACE TO MOD-TEMFSINF                                           
032900     .                                                                    
033000     EJECT                                                                
033100 B-KOLLA-NYCKLAR SECTION.                                                 
033200                                                                          
033210     MOVE JA                            TO NYCKLAR-SW                     
033220                                                                          
033230     MOVE ALL '+'           TO MSGI-WMSGINIT                              
033240     MOVE '001'             TO MSGI-KDCALL                                
033250     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033251     MOVE '4361'            TO MSGI-IDTRANS                               
033252     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033260     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033261                                                                          
033262     IF MSGI-IDLAND-SPR = 'GB'                                            
033263       MOVE +2 TO SPRAK-IX                                                
033264       MOVE 'GB ' TO MED-IDSKYLT                                          
033265     ELSE                                                                 
033266       MOVE 'S  ' TO MED-IDSKYLT                                          
033267       MOVE +1 TO SPRAK-IX                                                
033268     END-IF                                                               
033270                                                                          
033300     IF GODK-TRANS                                                        
033400       IF MID-IDPKLTAB-IN = ALL '+'                                       
033500          MOVE MID-IDPKLTAB-UT TO BASTAB                                  
033600          INSPECT BASTAB REPLACING LEADING SPACE BY ZERO                  
033700       ELSE                                                               
033800          MOVE MID-IDPKLTAB-IN TO BASTAB                                  
033900          INSPECT BASTAB REPLACING LEADING SPACE BY ZERO                  
034000       END-IF                                                             
034001                                                                          
034002       MOVE MSGI-IDDC               TO W-IDDC-B6                          
034003       PERFORM IMS-GU-WDB601                                              
034071                                                                          
034072       IF DCS-KDDC = SPACE OR DCS-DDC                                     
034079         MOVE NEJ                     TO NYCKLAR-SW                       
034080       END-IF                                                             
034081                                                                          
034090       IF NYCKLAR-OK                                                      
034091          MOVE DCS-IDDC       TO W-IDDC                                   
034092                                 W-IDDC-BAS                               
034093                                 W-IDDC-HLO                               
034094       ELSE                                                               
034095          MOVE '401' TO MED-IDMFSFEL                                      
034096          CALL WMEDKONV USING MED-WMEDAREA                                
034097          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
034098          PERFORM MFS-RENSA-FAELT-UT                                      
034099          PERFORM MFS-RENSA-FAELT-IN                                      
034100       END-IF                                                             
034101                                                                          
034110       MOVE LOW-VALUE TO XXKE-4442-LOW-VALUE                              
034200                         XXKF-4444-LOW-VALUE                              
034400       MOVE MFS-RENSA-FAELT TO MOD-IDPKLTAB-IN                            
034410                               MOD-IDDC-IN                                
034500                                                                          
034600       IF BASTAB NOT NUMERIC                                              
034700                                                                          
034800            MOVE NEJ TO NYCKLAR-SW                                        
034900            MOVE '001' TO MED-IDMFSFEL                                    
035000*******      TABELLID EJ NUMERISKT ***********************                
035100            CALL WMEDKONV USING MED-WMEDAREA                              
035200            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
035300            PERFORM MFS-RENSA-FAELT-UT                                    
035400            PERFORM MFS-RENSA-FAELT-IN                                    
035500            MOVE BASTAB     TO IDPKLTAB-WS                                
035600       ELSE                                                               
035700                                                                          
035800                                                                          
035900                                                                          
036000                                                                          
036100         IF BASTABELL                                                     
036200           IF MFS-UPDATE  AND MID-IDPKLTAB-IN = ALL '+'                   
036300                                                                          
036400              MOVE NEJ TO NYCKLAR-SW                                      
036500              MOVE '777' TO MED-IDMFSFEL                                  
036600*******        UPPDATERING EJ TILLÅTEN ***********************            
036700              CALL WMEDKONV USING MED-WMEDAREA                            
036800              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
036900              PERFORM MFS-ROER-EJ-FAELT-UT                                
037000              PERFORM MFS-RENSA-FAELT-IN                                  
037100              MOVE MID-IDPKLTAB-UT TO MOD-IDPKLTAB-UT                     
037200           END-IF                                                         
037300         END-IF                                                           
037310                                                                          
037400         IF NYCKLAR-OK                                                    
037410                                                                          
037500            IF MID-IDPKLTAB-IN = ALL '+'                                  
037600              MOVE MID-IDPKLTAB-UT TO IDPKLTAB-WS                         
037700              INSPECT IDPKLTAB-WS REPLACING LEADING SPACE BY ZERO         
037800              MOVE IDPKLTAB-WS TO W-IDPKLTAB                              
037900            ELSE                                                          
038000              MOVE MID-IDPKLTAB-IN TO IDPKLTAB-WS                         
038100              INSPECT IDPKLTAB-WS REPLACING LEADING SPACE BY ZERO         
038200              IF IDPKLTAB-WS NUMERIC                                      
038300                 MOVE IDPKLTAB-WS TO W-IDPKLTAB                           
038400                 MOVE '7' TO MFS-IDPFK                                    
038500                 MOVE SPACE TO MFS-KDTRTYP                                
038600              END-IF                                                      
038601            END-IF                                                        
038800         END-IF                                                           
038900       END-IF                                                             
039000                                                                          
039200       IF GODK-TRANS OR NYCKLAR-OK                                        
039210                                                                          
039300         MOVE IDPKLTAB-WS TO MOD-IDPKLTAB-UT                              
039400         INSPECT MOD-IDPKLTAB-UT REPLACING LEADING ZERO BY SPACE          
039401         IF MOD-IDPKLTAB-UT = '  '                                        
039402            MOVE ' 0'     TO MOD-IDPKLTAB-UT                              
039403         END-IF                                                           
039404                                                                          
039410         MOVE DCS-IDDC    TO MOD-IDDC-UT                                  
039420         INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE              
039500       END-IF                                                             
039600     ELSE                                                                 
039700       MOVE MFS-RENSA-FAELT TO MOD-IDPKLTAB-IN                            
039800                               MOD-IDPKLTAB-UT                            
039810                               MOD-IDDC-IN                                
039820                               MOD-IDDC-UT                                
039900       PERFORM MFS-RENSA-FAELT-UT                                         
040000       PERFORM MFS-RENSA-FAELT-IN                                         
040100       MOVE NEJ TO NYCKLAR-SW                                             
040200     END-IF                                                               
040300     .                                                                    
040400     EJECT                                                                
040500                                                                          
040600 E-SAMMA-SIDA SECTION.                                                    
040700                                                                          
040800     MOVE MID-IDRADNR-DOLD TO W-IDRADNR                                   
040900     IF MID-INDATA NOT = ALL '+' OR                                       
041000                MID-IDRADNR-IN NOT = ALL '+'                              
041100       MOVE NEJ TO INDATA-SW                                              
041200       MOVE '003' TO MED-IDMFSINF                                         
041300******* TRYCK PF11 VID UPPDATERING *************************              
041400       CALL WMEDKONV USING MED-WMEDAREA                                   
041500       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
041600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
041700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
041800       PERFORM MFS-LAS-IN-IGEN                                            
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
042200 F-LAES-VISA-XXKF-RADER SECTION.                                          
042300                                                                          
042400     IF MFS-FIRST                                                         
042500        MOVE ZERO TO W-IDRADNR                                            
042600     ELSE                                                                 
042700        IF MFS-NEXT                                                       
042800           MOVE MID-IDRADNR-DOLD-X TO W-IDRADNR                           
042900        ELSE                                                              
043000           MOVE MID-IDRADNR-DOLD   TO W-IDRADNR                           
043100        END-IF                                                            
043200     END-IF                                                               
043300                                                                          
043500     PERFORM IMS-GET-XXKF-RADER-FIRST                                     
043600     IF SEGMENT-FINNS                                                     
043700       MOVE XXKF-4444-IDRADNR TO MOD-IDRADNR-DOLD                         
043800                                 MOD-IDRADNR-DOLD-X                       
043900     ELSE                                                                 
044000       MOVE ZERO TO MOD-IDRADNR-DOLD                                      
044100     END-IF                                                               
044200                                                                          
044210     MOVE +1 TO INDX                                                      
044300     PERFORM UNTIL INDX > MAX-RADER                                       
044400       IF SEGMENT-FINNS                                                   
044500         MOVE XXKF-4444-IDRADNR    TO MOD-IDRADNR(INDX)                   
044600         INSPECT MOD-IDRADNR(INDX) REPLACING LEADING ZERO BY SPACE        
044700         MOVE XXKF-4444-KDORDKL    TO MOD-KDORDKL(INDX)                   
044800        IF XXKF-4444-KVRADER = KVRADER-KOLL                               
044900           MOVE MFS-RENSA-FAELT TO MOD-KVRADER(INDX)                      
045000        ELSE                                                              
045100           MOVE XXKF-4444-KVRADER  TO MOD-KVRADER(INDX)                   
045200        END-IF                                                            
045300        IF XXKF-4444-VKORDNTO = VKORDNTO-KOLL                             
045400           MOVE MFS-RENSA-FAELT TO MOD-VKORDNTO(INDX)                     
045500        ELSE                                                              
045600           MOVE XXKF-4444-VKORDNTO TO VKORDNTO-X                          
045700           MOVE VKORDNTO-X         TO MOD-VKORDNTO(INDX)                  
045800           INSPECT MOD-VKORDNTO(INDX)                                     
045900                   REPLACING LEADING ZERO BY SPACE                        
046000        END-IF                                                            
046100        IF XXKF-4444-VLORDNTO = VLORDNTO-KOLL                             
046200           MOVE MFS-RENSA-FAELT TO MOD-VLORDNTO(INDX)                     
046300        ELSE                                                              
046400           MOVE XXKF-4444-VLORDNTO TO VLORDNTO-X                          
046500           MOVE VLORDNTO-X         TO MOD-VLORDNTO(INDX)                  
046600           INSPECT MOD-VLORDNTO(INDX)                                     
046700                   REPLACING LEADING ZERO BY SPACE                        
046800        END-IF                                                            
046900         MOVE XXKF-4444-IDHLOTAB   TO MOD-IDHLOTAB(INDX)                  
047000         MOVE XXKF-4444-KDPRODKL   TO MOD-KDPRODKL(INDX)                  
047100         PERFORM IMS-GET-XXKF-RADER                                       
047200       ELSE                                                               
047300         MOVE MFS-RENSA-FAELT      TO MOD-IDRADNR(INDX)                   
047400                                      MOD-KDORDKL(INDX)                   
047500                                      MOD-KVRADER(INDX)                   
047600                                      MOD-VKORDNTO(INDX)                  
047700                                      MOD-VLORDNTO(INDX)                  
047800                                      MOD-IDHLOTAB(INDX)                  
047900                                      MOD-KDPRODKL(INDX)                  
048000                                                                          
048100       END-IF                                                             
048200       ADD +1 TO INDX                                                     
048300     END-PERFORM                                                          
048400                                                                          
048500     IF SEGMENT-FINNS                                                     
048600       MOVE XXKF-4444-IDRADNR TO MOD-IDRADNR-DOLD-X                       
048700       IF MOD-TEMFSINF = SPACE                                            
048800          MOVE '105' TO MED-IDMFSINF                                      
048900***********    MER INFORMATION FINNS, TRYCK PF8 ******************        
049000          CALL WMEDKONV USING MED-WMEDAREA                                
049100          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
049200       END-IF                                                             
049300     END-IF                                                               
049400     IF INDATA-OK                                                         
049500        PERFORM MFS-RENSA-FAELT-IN                                        
049600     END-IF                                                               
049700     .                                                                    
049800     EJECT                                                                
049900 G-KOLLA-INPUT SECTION.                                                   
050000     MOVE MID-IDRADNR-IN TO DUBLETT-RAD                                   
050100     IF RADNUMMER-FEL                                                     
050200        IF MID-KDORDKL-IN      = ALL '+' AND                              
050300           MID-KVRADER-IN      = ALL '+' AND                              
050400           MID-VKORDNTO-IN     = ALL '+' AND                              
050500           MID-VLORDNTO-IN     = ALL '+'                                  
050600                                                                          
050700           MOVE JA TO INDATA-SW                                           
050800           MOVE '00120' TO DUBLETT-RAD                                    
050900        ELSE                                                              
051000           MOVE '007' TO MED-IDMFSFEL                                     
051100***** OTILLÅTEN UPPDATERING, BAS-RAD FÅR EJ ÄNDRAS ****                   
051200           CALL WMEDKONV USING MED-WMEDAREA                               
051300           MOVE MED-MFSFEL               TO MOD-TEMFSFEL                  
051310**         MOVE '***1'                   TO MOD-TEMFSINF                  
051400           PERFORM MFS-ROER-EJ-FAELT-IN                                   
051500           PERFORM MFS-ROER-EJ-FAELT-UT                                   
051600           MOVE NEJ TO INDATA-SW                                          
051700           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-IN-ATTR                  
051800        END-IF                                                            
051900     ELSE                                                                 
052000       MOVE JA TO INDATA-SW                                               
052100       IF SEGMENT-FINNS                                                   
052102          IF MID-IDRADNR-IN = ALL '+'                                     
052103            IF MID-INDATA = ALL '+'                                       
052104              MOVE '011' TO MED-IDMFSINF                                  
052105***********   PF11 OCH TOM INDATARAD *************************            
052106            ELSE                                                          
052107              MOVE '014' TO MED-IDMFSINF                                  
052108***********   PF11 OCH INGET RADNR FÖR INDATARAD *************            
052110            END-IF                                                        
052500            CALL WMEDKONV USING MED-WMEDAREA                              
052600            MOVE MED-MFSINF TO MOD-TEMFSINF                               
052700            PERFORM MFS-ROER-EJ-FAELT-UT                                  
052800            PERFORM MFS-ROER-EJ-FAELT-IN                                  
052900            MOVE 'W' TO INDATA-SW                                         
053000          ELSE                                                            
053100            PERFORM GA-KOLLA-VLORDNTO                                     
053200          END-IF                                                          
053300            IF MID-IDRADNR-IN NOT = ALL '+'                               
053400              PERFORM IMS-GET-XXKF-RADER-LAST                             
053500              IF MID-IDRADNR-IN NOT NUMERIC OR                            
053600                  MID-IDRADNR-IN NOT > ZERO OR                            
053700                  MID-IDRADNR-IN NOT < XXKF-4444-IDRADNR                  
053800                                                                          
053900                MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-IN-ATTR             
054000                MOVE NEJ TO INDATA-SW                                     
054100                MOVE NEJ TO RADNUMMER-SW                                  
054200              ELSE                                                        
054300                MOVE MID-IDRADNR-IN TO W-IDRADNR                          
054400                PERFORM IMS-GET-XXKF-RADER-FIRST                          
054500                MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-IN-ATTR           
054600              END-IF                                                      
054700            ELSE                                                          
054800              MOVE NEJ TO RADNUMMER-SW                                    
054900            END-IF                                                        
055000                                                                          
055100            EJECT                                                         
055200                                                                          
055300            IF MID-KDPRODKL-IN NOT = ALL '+'                              
055400              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRODKL-IN-ATTR           
055500            ELSE                                                          
055600              IF RADNUMMER-FINNS                                          
055700                IF MID-INDATA NOT = ALL '+' AND                           
055800                  MID-IDRADNR-IN NOT = XXKF-4444-IDRADNR                  
055900                  MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPRODKL-IN-ATTR         
056000                  MOVE NEJ TO INDATA-SW                                   
056100                END-IF                                                    
056200              END-IF                                                      
056300            END-IF                                                        
056400                                                                          
056500                                                                          
056600            IF MID-KVRADER-IN NOT = ALL '+'                               
056700              IF MID-KVRADER-IN NOT NUMERIC                               
056800                                                                          
056900                MOVE MFS-NUM-FAELT-FEL TO MOD-KVRADER-IN-ATTR             
057000                MOVE NEJ TO INDATA-SW                                     
057100              ELSE                                                        
057200                MOVE MFS-NUM-FAELT-RAETT TO MOD-KVRADER-IN-ATTR           
057300              END-IF                                                      
057400            END-IF                                                        
057500                                                                          
057600                                                                          
057700            IF RADNUMMER-FINNS                                            
057800              IF MID-KDORDKL-IN NOT = ALL '+'                             
057900                IF MID-KDORDKL-IN NOT NUMERIC                             
058000                   MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDKL-IN-ATTR          
058100                   MOVE NEJ TO INDATA-SW                                  
058200                ELSE                                                      
058300                  PERFORM IMS-GET-XXKF-RADER-FIRST                        
058400                  MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-IN-ATTR         
058500                 IF MID-KDORDKL-IN NOT = XXKF-4444-KDORDKL AND            
058600                  MID-IDRADNR-IN NOT = XXKF-4444-IDRADNR                  
058700                   MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDKL-IN-ATTR          
058800                   MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-IN-ATTR          
058900                   MOVE NEJ TO INDATA-SW                                  
059000                 ELSE                                                     
059100                  MOVE MFS-NUM-FAELT-RAETT TO MOD-KDORDKL-IN-ATTR         
059200                 END-IF                                                   
059300                END-IF                                                    
059400              ELSE                                                        
059500                IF MID-IDRADNR-IN NOT = XXKF-4444-IDRADNR                 
059600                   MOVE NEJ TO INDATA-SW                                  
059700                   MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDKL-IN-ATTR          
059800                END-IF                                                    
059900              END-IF                                                      
060000            END-IF                                                        
060100            EJECT                                                         
060200                                                                          
060300            PERFORM GB-KOLLA-VKORDNTO                                     
060400                                                                          
060500            PERFORM GC-KOLLA-IDHLOTAB                                     
060600                                                                          
060700            IF INDATA-FEL                                                 
060800              MOVE '001' TO MED-IDMFSFEL                                  
060900***********   KORRIGERA UPPLYSTA FÄLT ***************************         
061000              CALL WMEDKONV USING MED-WMEDAREA                            
061100              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
061200              PERFORM MFS-ROER-EJ-FAELT-UT                                
061300              PERFORM MFS-ROER-EJ-FAELT-IN                                
061400            END-IF                                                        
061500               IF INDATA-SW = 'W'                                         
061600                  MOVE NEJ TO INDATA-SW                                   
061700                  MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                    
061800                MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-IN-ATTR           
061900               END-IF                                                     
062000       END-IF                                                             
062100     END-IF                                                               
062200                                                                          
062300     .                                                                    
062400     EJECT                                                                
062500 GA-KOLLA-VLORDNTO SECTION.                                               
062600                                                                          
062700     IF MID-VLORDNTO-IN NOT = ALL '+'                                     
062800       MOVE MID-VLORDNTO-IN TO DEC-IDFRIDATA                              
062900       MOVE +4 TO DEC-KVHELTAL                                            
063000       MOVE +3 TO DEC-KVDECIMAL                                           
063100       CALL WDECEDIT USING DEC-WDECAREA                                   
063200       MOVE DEC-IDEDITDATA TO VLORDNTO-X VLORDNTO-X-SIGN                  
063300       IF DEC-KDSVAR-FEL                                                  
063400         MOVE MFS-NUM-FAELT-FEL TO MOD-VLORDNTO-IN-ATTR                   
063500         MOVE NEJ TO INDATA-SW                                            
063600       END-IF                                                             
063700     ELSE                                                                 
063800        MOVE MFS-NUM-FAELT-RAETT TO MOD-VLORDNTO-IN-ATTR                  
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200 GB-KOLLA-VKORDNTO SECTION.                                               
064300                                                                          
064400     IF MID-VKORDNTO-IN NOT = ALL '+'                                     
064500        MOVE MID-VKORDNTO-IN TO DEC-IDFRIDATA                             
064600        MOVE +6 TO DEC-KVHELTAL                                           
064700        MOVE +1 TO DEC-KVDECIMAL                                          
064800        CALL WDECEDIT USING DEC-WDECAREA                                  
064900        MOVE DEC-IDEDITDATA TO VKORDNTO-X VKORDNTO-X-SIGN                 
065000        IF DEC-KDSVAR-FEL                                                 
065100          MOVE MFS-NUM-FAELT-FEL TO MOD-VKORDNTO-IN-ATTR                  
065200          MOVE NEJ TO INDATA-SW                                           
065300        END-IF                                                            
065400     ELSE                                                                 
065500         MOVE MFS-NUM-FAELT-RAETT TO MOD-VKORDNTO-IN-ATTR                 
065600     END-IF                                                               
065700                                                                          
065800     .                                                                    
065900     EJECT                                                                
066000 GC-KOLLA-IDHLOTAB SECTION.                                               
066100                                                                          
066200     IF MID-IDHLOTAB-IN NOT = ALL '+'                                     
066300         MOVE MID-IDHLOTAB-IN TO W-IDHLOTAB                               
066400         PERFORM IMS-GET-XXKE-HLO-UNIK                                    
066500       IF SEGMENT-FINNS                                                   
066600          IF MID-IDHLOTAB-IN NOT NUMERIC                                  
066700             MOVE MFS-NUM-FAELT-FEL TO MOD-IDHLOTAB-IN-ATTR               
066800             MOVE NEJ TO INDATA-SW                                        
066900           ELSE                                                           
067000             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDHLOTAB-IN-ATTR             
067100           END-IF                                                         
067200       ELSE                                                               
067300             MOVE NEJ TO INDATA-SW                                        
067400             MOVE MFS-NUM-FAELT-FEL TO MOD-IDHLOTAB-IN-ATTR               
067500       END-IF                                                             
067600     ELSE                                                                 
067700       IF RADNUMMER-FINNS                                                 
067800         IF MID-INDATA NOT = ALL '+' AND                                  
067900                  MID-IDRADNR-IN NOT = XXKF-4444-IDRADNR                  
068000           MOVE NEJ TO INDATA-SW                                          
068100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDHLOTAB-IN-ATTR                 
068200         END-IF                                                           
068300       END-IF                                                             
068400     END-IF                                                               
068500                                                                          
068600     .                                                                    
068700     EJECT                                                                
068800 H-UPPDATERA SECTION.                                                     
068900                                                                          
069000     PERFORM IMS-GET-XXKF-TABELL                                          
069100     IF SEGMENT-FINNS                                                     
069200        MOVE MID-IDRADNR-IN              TO W-IDRADNR                     
069300                                                                          
069400        IF MID-INDATA NOT = ALL '+'                                       
069500           PERFORM IMS-GET-XXKF-RADER-UNIK                                
069600           IF SEGMENT-SAKNAS                                              
069700              MOVE MID-IDRADNR-IN        TO W-IDRADNR                     
069800                                            XXKF-4444-IDRADNR             
069900                                            MOD-IDRADNR-DOLD              
070000                                            MID-IDRADNR-DOLD              
070100              MOVE KVRADER-KOLL          TO XXKF-4444-KVRADER             
070200              MOVE VLORDNTO-KOLL         TO XXKF-4444-VLORDNTO            
070300              MOVE VKORDNTO-KOLL         TO XXKF-4444-VKORDNTO            
070400              MOVE LOW-VALUE TO XXKF-4444-LOW-VALUE                       
070500              MOVE NEJ TO AENDRING-SW                                     
070600              PERFORM IMS-ISRT-XXKF-RADER                                 
070700           END-IF                                                         
070800                                                                          
070900           PERFORM HB-AENDRING-AV-RAD                                     
071000                                                                          
071100        ELSE                                                              
071200                                                                          
071300           PERFORM HC-BORTTAG-AV-RAD                                      
071400                                                                          
071500        END-IF                                                            
071600                                                                          
071700     ELSE                                                                 
071800                                                                          
071900        PERFORM HD-TILLAEGG-AV-TABELL                                     
072000                                                                          
072100        IF MID-INDATA NOT = ALL '+'                                       
072200                      AND  MID-IDRADNR-IN NOT = ALL '+'                   
072300           MOVE MID-IDRADNR-IN           TO W-IDRADNR                     
072400                                         XXKF-4444-IDRADNR                
072500                                         MOD-IDRADNR(INDX)                
072600           PERFORM IMS-GET-XXKF-RADER-UNIK                                
072700           IF SEGMENT-SAKNAS                                              
072800              PERFORM HA-FLYTTA-DATA-TILL-MODEN                           
072900              PERFORM IMS-ISRT-XXKF-RADER                                 
073000              PERFORM IMS-GET-XXKF-TABELL                                 
073100           END-IF                                                         
073200        END-IF                                                            
073300     END-IF                                                               
073400     PERFORM IMS-GET-XXKF-TABELL                                          
073500     .                                                                    
073600     EJECT                                                                
073700 HA-FLYTTA-DATA-TILL-MODEN SECTION.                                       
073800                                                                          
073900     IF NOT AENDRING AND MID-KDORDKL-IN NOT = ALL '+'                     
074000        MOVE ZERO TO MOD-KDORDKL(INDX) XXKF-4444-KDORDKL                  
074100        MOVE MID-KDORDKL-IN TO MOD-KDORDKL(INDX)                          
074200                               XXKF-4444-KDORDKL                          
074300        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDORDKL-ATTR(INDX)              
074400                                      MOD-IDRADNR-ATTR(INDX)              
074500        MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-IN                            
074600     END-IF                                                               
074700                                                                          
074800     IF MID-KVRADER-IN NOT = ALL '+'                                      
074900        MOVE MID-KVRADER-IN TO  XXKF-4444-KVRADER                         
075000                                MOD-KVRADER(INDX)                         
075100        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVRADER-ATTR(INDX)              
075200        MOVE MFS-RENSA-FAELT TO MOD-KVRADER-IN                            
075300     ELSE                                                                 
075400        MOVE MFS-ROER-EJ-FAELT TO MOD-KVRADER-IN                          
075500     END-IF                                                               
075600                                                                          
075700                                                                          
075800     IF MID-VKORDNTO-IN NOT = ALL '+'                                     
075900        MOVE VKORDNTO-X      TO MOD-VKORDNTO(INDX)                        
076000        MOVE VKORDNTO-X-SIGN TO XXKF-4444-VKORDNTO                        
076100        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKORDNTO-ATTR(INDX)             
076200        MOVE MFS-RENSA-FAELT TO MOD-VKORDNTO-IN                           
076300     ELSE                                                                 
076400        MOVE MFS-ROER-EJ-FAELT TO MOD-VKORDNTO-IN                         
076500     END-IF                                                               
076600                                                                          
076700                                                                          
076800     IF MID-VLORDNTO-IN NOT = ALL '+'                                     
076900        MOVE VLORDNTO-X      TO MOD-VLORDNTO(INDX)                        
077000        MOVE VLORDNTO-X-SIGN TO XXKF-4444-VLORDNTO                        
077100        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VLORDNTO-ATTR(INDX)             
077200        MOVE MFS-RENSA-FAELT TO MOD-VLORDNTO-IN                           
077300     ELSE                                                                 
077400        MOVE MFS-ROER-EJ-FAELT TO MOD-VLORDNTO-IN                         
077500     END-IF                                                               
077600                                                                          
077700     IF MID-KDPRODKL-IN NOT = ALL '+'                                     
077800        MOVE MID-KDPRODKL-IN TO XXKF-4444-KDPRODKL                        
077900                                MOD-KDPRODKL(INDX)                        
078000        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRODKL-ATTR(INDX)             
078100        MOVE MFS-RENSA-FAELT TO MOD-KDPRODKL-IN                           
078200     ELSE                                                                 
078300        MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRODKL-IN                         
078400     END-IF                                                               
078500                                                                          
078600     IF MID-IDHLOTAB-IN NOT = ALL '+'                                     
078700        MOVE MID-IDHLOTAB-IN TO XXKF-4444-IDHLOTAB                        
078800                                MOD-IDHLOTAB(INDX)                        
078900        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDHLOTAB-ATTR(INDX)             
079000        MOVE MFS-RENSA-FAELT TO MOD-IDHLOTAB-IN                           
079100     END-IF                                                               
079200     EJECT                                                                
079300     .                                                                    
079400                                                                          
079500                                                                          
079600 HB-AENDRING-AV-RAD SECTION.                                              
079700                                                                          
079800     IF AENDRING AND MID-KDORDKL-IN NOT = ALL '+'                         
079810       CONTINUE                                                           
079900       MOVE '007' TO MED-IDMFSFEL                                         
080000*************** OTILLÅTEN UPPDATERING, BAS-RAD FÅR EJ ÄNDRAS ****         
080100       CALL WMEDKONV USING MED-WMEDAREA                                   
080200       MOVE MED-MFSFEL                   TO MOD-TEMFSFEL                  
080210**     MOVE '***2'                       TO MOD-TEMFSINF                  
080300       PERFORM MFS-FORM-ATTR-IN                                           
080400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
080500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
080600       MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDKL-IN-ATTR                      
080700       MOVE NEJ TO INDATA-SW                                              
080800     ELSE                                                                 
080900       PERFORM IMS-GET-XXKF-TABELL                                        
081000       PERFORM F-LAES-VISA-XXKF-RADER                                     
081100       PERFORM IMS-GET-XXKF-RADER-FIRST                                   
081200       MOVE +1 TO INDX                                                    
081300                                                                          
081400       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
081410                     XXKF-4444-IDRADNR = MID-IDRADNR-IN                   
081600          PERFORM IMS-GET-XXKF-RADER                                      
081700          ADD +1 TO INDX                                                  
081800       END-PERFORM                                                        
081900                                                                          
082000       IF  NOT RADNUMMER-FEL        AND                                   
082100           INDX NOT > MAX-RADER     AND                                   
082200           MID-IDRADNR-IN IS GREATER THAN OR EQUAL TO                     
082300           MID-IDRADNR-DOLD                                               
082400                                                                          
082500          MOVE MID-IDRADNR-IN TO W-IDRADNR                                
082600          PERFORM IMS-GET-XXKF-RADER-UNIK                                 
082700          PERFORM HA-FLYTTA-DATA-TILL-MODEN                               
082800          PERFORM IMS-REPL-XXKF-RADER                                     
082900          PERFORM IMS-GET-XXKF-TABELL                                     
083000          PERFORM F-LAES-VISA-XXKF-RADER                                  
083100          IF AENDRING                                                     
083200              MOVE '779' TO MED-IDMFSINF                                  
083300********************   ÄNDRING UTFÖRD **************************          
083400          ELSE                                                            
083500            MOVE '101' TO MED-IDMFSINF                                    
083600*******************UPPDATERING   UTFÖRD*************************          
083700          END-IF                                                          
083800          CALL WMEDKONV USING MED-WMEDAREA                                
083900          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
084000          PERFORM MFS-FORM-ATTR-IN                                        
084100       ELSE                                                               
084110                                                                          
084200          MOVE '007' TO MED-IDMFSFEL                                      
084300***************   OTILLÅTEN UPPDATERING, BAS-RAD FÅR EJ ÄNDRAS ***        
084400          CALL WMEDKONV USING MED-WMEDAREA                                
084500          MOVE MED-MFSFEL                TO MOD-TEMFSFEL                  
084510**        MOVE '***3'                    TO MOD-TEMFSINF                  
084600          MOVE    MFS-NUM-FAELT-FEL TO MOD-IDRADNR-IN-ATTR                
084700          PERFORM MFS-ROER-EJ-FAELT-IN                                    
084800       END-IF                                                             
084900       MOVE NEJ TO INDATA-SW                                              
085000     END-IF                                                               
085100     .                                                                    
085200     EJECT                                                                
085300                                                                          
085400 HC-BORTTAG-AV-RAD SECTION.                                               
085500                                                                          
085600     MOVE JA TO ALLT-SW                                                   
085700     IF MID-IDRADNR-DOLD NOT = MID-IDRADNR-DOLD-X                         
085800        IF MID-IDRADNR-IN IS NOT LESS THAN MID-IDRADNR-DOLD-X             
085900           MOVE NEJ TO ALLT-SW                                            
086000        END-IF                                                            
086100     END-IF                                                               
086200     MOVE MID-IDRADNR-IN TO DUBLETT-RAD                                   
086300     IF  NOT RADNUMMER-FEL        AND                                     
086400         ALLT-SW = JA             AND                                     
086500         MID-IDRADNR-IN IS GREATER THAN OR EQUAL TO                       
086600         MID-IDRADNR-DOLD                                                 
086700                                                                          
086800        PERFORM IMS-GET-XXKF-RADER-UNIK                                   
086900        IF SEGMENT-FINNS                                                  
087000           PERFORM IMS-DLET-XXKF-RADER                                    
087100           MOVE '752' TO MED-IDMFSINF                                     
087200*************** RADEN ANNULERAD ***************************               
087300           CALL WMEDKONV USING MED-WMEDAREA                               
087400           MOVE MED-MFSINF TO MOD-TEMFSINF                                
087500           PERFORM MFS-FORM-ATTR-IN                                       
087600           PERFORM MFS-ROER-EJ-FAELT-IN                                   
087700        ELSE                                                              
087800           MOVE '014' TO MED-IDMFSFEL                                     
087900*************** FEL RADNUMMER *************************      CL1          
088000           CALL WMEDKONV USING MED-WMEDAREA                               
088100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
088200           PERFORM MFS-FORM-ATTR-IN                                       
088300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
088400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
088500           MOVE NEJ TO INDATA-SW                                          
088600           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-IN-ATTR                  
088700        END-IF                                                            
088800        PERFORM IMS-GET-XXKF-TABELL                                       
088900     ELSE                                                                 
089000        MOVE '066' TO MED-IDMFSFEL                                        
089100******* ANNULLATION EJ MÖJLIG, BAS-RAD FÅR EJ TAS BORT*********           
089200        CALL WMEDKONV USING MED-WMEDAREA                                  
089300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
089400        PERFORM MFS-FORM-ATTR-IN                                          
089500        MOVE NEJ TO INDATA-SW                                             
089600        PERFORM MFS-ROER-EJ-FAELT-IN                                      
089700        PERFORM MFS-ROER-EJ-FAELT-UT                                      
089800        MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-IN-ATTR                     
089900     END-IF                                                               
090000     .                                                                    
090100     EJECT                                                                
090200                                                                          
090300 HD-TILLAEGG-AV-TABELL SECTION.                                           
090400                                                                          
090500     MOVE W-IDHTYP-X TO XXKF-4443-WDGX4443                                
090600     PERFORM IMS-ISRT-XXKF-TABELL                                         
090700     MOVE +1 TO INDX                                                      
090800     PERFORM IMS-GET-XXKF-BASTABELL                                       
090900     IF SEGMENT-FINNS                                                     
091000        MOVE ZERO TO W-IDRADNR                                            
091100        PERFORM IMS-GET-XXKF-RADER-BAS                                    
091200           PERFORM UNTIL SEGMENT-SAKNAS                                   
091300              MOVE XXKF-4444-IDRADNR TO TAB-IDRADNR(INDX)                 
091400              MOVE XXKF-4444-KDORDKL TO TAB-KDORDKL(INDX)                 
091500              MOVE XXKF-4444-KVRADER TO TAB-KVRADER(INDX)                 
091600              MOVE XXKF-4444-VKORDNTO TO TAB-VKORDNTO(INDX)               
091700              MOVE XXKF-4444-VLORDNTO TO TAB-VLORDNTO(INDX)               
091800              MOVE XXKF-4444-IDHLOTAB TO TAB-IDHLOTAB(INDX)               
091900              MOVE XXKF-4444-KDPRODKL TO TAB-KDPRODKL(INDX)               
092000              PERFORM IMS-GET-XXKF-RADER-BAS                              
092100              ADD +1 TO INDX                                              
092200           END-PERFORM                                                    
092300           COMPUTE MAX-RADER-I-BASTABELL  = INDX - 1                      
092400           MOVE +1 TO INDX BAS-INDX                                       
092500           PERFORM IMS-GET-XXKF-TABELL                                    
092600           PERFORM UNTIL INDX > MAX-RADER-I-BASTABELL                     
092700              MOVE HELP-TABELL(INDX)      TO XXKF-4444-WDGX4444           
092800              PERFORM IMS-ISRT-XXKF-RADER                                 
092900              ADD +1 TO INDX BAS-INDX                                     
093000           END-PERFORM                                                    
093100     END-IF                                                               
093200     MOVE '024' TO MED-IDMFSINF                                           
093300*************** GENERELL TABELL UPPLAGD *******************               
093400     CALL WMEDKONV USING MED-WMEDAREA                                     
093500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
093600     .                                                                    
093700     EJECT                                                                
093800                                                                          
093900 MFS-RENSA-FAELT-UT SECTION.                                              
094000                                                                          
094100     MOVE +1 TO INDX                                                      
094200     PERFORM UNTIL INDX > MAX-RADER                                       
094300       MOVE MFS-RENSA-FAELT TO MOD-IDRADNR(INDX)                          
094400                               MOD-KDORDKL(INDX)                          
094500                               MOD-KVRADER(INDX)                          
094600                               MOD-VKORDNTO(INDX)                         
094700                               MOD-VLORDNTO(INDX)                         
094800                               MOD-KDPRODKL(INDX)                         
094900                               MOD-IDHLOTAB(INDX)                         
095000       ADD +1 TO INDX                                                     
095100     END-PERFORM                                                          
095200     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-DOLD                             
095300                             MOD-IDRADNR-DOLD-X                           
095400     .                                                                    
095500     SKIP2                                                                
095600 MFS-RENSA-FAELT-IN SECTION.                                              
095700                                                                          
095800     MOVE MFS-RENSA-FAELT   TO MOD-IDRADNR-IN                             
095900                               MOD-KDORDKL-IN                             
096000                               MOD-KVRADER-IN                             
096100                               MOD-VKORDNTO-IN                            
096200                               MOD-VLORDNTO-IN                            
096300                               MOD-KDPRODKL-IN                            
096400                               MOD-IDHLOTAB-IN                            
096500     .                                                                    
096600     EJECT                                                                
096700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
096800                                                                          
096900     MOVE +1 TO INDX                                                      
097000     PERFORM UNTIL INDX > MAX-RADER                                       
097100        MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR(INDX)                       
097200                                  MOD-KDORDKL(INDX)                       
097300                                  MOD-KVRADER(INDX)                       
097400                                  MOD-VKORDNTO(INDX)                      
097500                                  MOD-VLORDNTO(INDX)                      
097600                                  MOD-KDPRODKL(INDX)                      
097700                                  MOD-IDHLOTAB(INDX)                      
097800       ADD +1 TO INDX                                                     
097900     END-PERFORM                                                          
098000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-DOLD                           
098100                               MOD-IDRADNR-DOLD-X                         
098200     .                                                                    
098300     SKIP2                                                                
098400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
098500                                                                          
098600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-IN                             
098700                               MOD-KDORDKL-IN                             
098800                               MOD-KVRADER-IN                             
098900                               MOD-VKORDNTO-IN                            
099000                               MOD-VLORDNTO-IN                            
099100                               MOD-KDPRODKL-IN                            
099200                               MOD-IDHLOTAB-IN                            
099300     .                                                                    
099400     EJECT                                                                
099500 MFS-FORM-ATTR-IN SECTION.                                                
099600                                                                          
099700     MOVE MFS-FORMATETS-ATTR TO MOD-IDRADNR-IN-ATTR                       
099800                                MOD-KDORDKL-IN-ATTR                       
099900                                MOD-KVRADER-IN-ATTR                       
100000                                MOD-VKORDNTO-IN-ATTR                      
100100                                MOD-VLORDNTO-IN-ATTR                      
100200                                MOD-KDPRODKL-IN-ATTR                      
100300                                MOD-IDHLOTAB-IN-ATTR                      
100400                                                                          
100500     .                                                                    
100600     SKIP2                                                                
100700 MFS-LAS-IN-IGEN SECTION.                                                 
100800                                                                          
100900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDRADNR-IN-ATTR                    
101000                                   MOD-KDORDKL-IN-ATTR                    
101100                                   MOD-KVRADER-IN-ATTR                    
101200                                   MOD-VKORDNTO-IN-ATTR                   
101300                                   MOD-VLORDNTO-IN-ATTR                   
101400                                   MOD-KDPRODKL-IN-ATTR                   
101500                                   MOD-IDHLOTAB-IN-ATTR                   
101600                                                                          
101700     .                                                                    
101800     EJECT                                                                
101900* IMS SEKTIONER                                                           
102000     SKIP3                                                                
102100 IMS-GET-MSG SECTION.                                                     
102200                                                                          
102300     MOVE '  QC' TO GODK-STATUSKODER                                      
102400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
102500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
102600     PERFORM IMS-STATUSKONTROLL                                           
102700     .                                                                    
102800     SKIP3                                                                
102900 IMS-INSERT-MSG SECTION.                                                  
103000                                                                          
103010     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
103200       MOVE '0' TO MFS-KDHUVOMR                                           
103300     END-IF                                                               
103400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
103500     MOVE SPACE TO GODK-STATUSKODER                                       
103600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
103700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103800     PERFORM IMS-STATUSKONTROLL                                           
103900     .                                                                    
104000     EJECT                                                                
104100 IMS-GET-XXKF-TABELL SECTION.                                             
104200                                                                          
104300     STRING 'WLXXKF01(WDGXKEY  =' W-IDHTYP-X ')'                          
104400          DELIMITED BY SIZE INTO SSA1                                     
104500     MOVE '  GE' TO GODK-STATUSKODER                                      
104600     CALL CBLTDLI USING GU XXKF-PCB DLI-IO-AREA SSA1                      
104700     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
104800     PERFORM IMS-STATUSKONTROLL                                           
104900     .                                                                    
105000                                                                          
105100                                                                          
105200 IMS-GET-XXKF-BASTABELL SECTION.                                          
105300                                                                          
105400     STRING 'WLXXKF01(WDGXKEY  =' W-IDHTYP-X-BAS ')'                      
105500          DELIMITED BY SIZE INTO SSA1                                     
105600     MOVE '  GE' TO GODK-STATUSKODER                                      
105700     CALL CBLTDLI USING GHU XXKF-PCB DLI-IO-AREA SSA1                     
105800     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
105900     PERFORM IMS-STATUSKONTROLL                                           
106000     .                                                                    
106100                                                                          
106200 IMS-GET-XXKE-HLO-UNIK  SECTION.                                          
106300                                                                          
106400     STRING 'WLXXKE01(WDGXKEY  =' W-IDHLOTAB-ROT ')'                      
106500          DELIMITED BY SIZE INTO SSA1                                     
106600     STRING 'WLXXKE11(WDGXKEY  =' W-IDHLOTAB-X ')'                        
106700          DELIMITED BY SIZE INTO SSA2                                     
106800     MOVE '  GE' TO GODK-STATUSKODER                                      
106900     CALL CBLTDLI USING GHU XXKE-PCB DLI-IO-AREA SSA1 SSA2                
107000     MOVE XXKE-STATUS-CODE TO STATUS-WS                                   
107100     PERFORM IMS-STATUSKONTROLL                                           
107200     .                                                                    
107300                                                                          
107400 IMS-REPL-XXKF-RADER SECTION.                                             
107500                                                                          
107600     MOVE '  ' TO GODK-STATUSKODER                                        
107700     CALL CBLTDLI USING REPL XXKF-PCB DLI-IO-AREA                         
107800     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
107900     PERFORM IMS-STATUSKONTROLL                                           
108000     .                                                                    
108100     EJECT                                                                
108200 IMS-GET-XXKF-RADER SECTION.                                              
108300                                                                          
108400     MOVE 'WLXXKF11 ' TO SSA1                                             
108500     MOVE '  GE' TO GODK-STATUSKODER                                      
108600     CALL CBLTDLI USING GNP XXKF-PCB DLI-IO-AREA SSA1                     
108700     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
108800     PERFORM IMS-STATUSKONTROLL                                           
108900     .                                                                    
109000                                                                          
109100 IMS-GET-XXKF-RADER-BAS SECTION.                                          
109200                                                                          
109300     STRING 'WLXXKF01(WDGXKEY  =' W-IDHTYP-X-BAS ')'                      
109400          DELIMITED BY SIZE INTO SSA1                                     
109500     STRING 'WLXXKF11(WDGXKEY =>' W-IDRADNR-X ')'                         
109600          DELIMITED BY SIZE INTO SSA2                                     
109700     MOVE '  GE' TO GODK-STATUSKODER                                      
109800     CALL CBLTDLI USING GNP XXKF-PCB DLI-IO-AREA SSA1 SSA2                
109900     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
110000     PERFORM IMS-STATUSKONTROLL                                           
110100     .                                                                    
110200                                                                          
110300 IMS-GET-XXKF-RADER-FIRST SECTION.                                        
110400                                                                          
110500     STRING 'WLXXKF11*F(WDGXKEY =>' W-IDRADNR-X ')'                       
110600          DELIMITED BY SIZE INTO SSA1                                     
110700     MOVE '  GE' TO GODK-STATUSKODER                                      
110800     CALL CBLTDLI USING GNP XXKF-PCB DLI-IO-AREA SSA1                     
110900     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
111000     PERFORM IMS-STATUSKONTROLL                                           
111100     .                                                                    
111200                                                                          
111300                                                                          
111400 IMS-GET-XXKF-RADER-UNIK SECTION.                                         
111500                                                                          
111600     STRING 'WLXXKF01(WDGXKEY  =' W-IDHTYP-X ')'                          
111700          DELIMITED BY SIZE INTO SSA1                                     
111800     STRING 'WLXXKF11(WDGXKEY  =' W-IDRADNR-X ')'                         
111900          DELIMITED BY SIZE INTO SSA2                                     
112000     MOVE '  GE' TO GODK-STATUSKODER                                      
112100     CALL CBLTDLI USING GHU XXKF-PCB DLI-IO-AREA SSA1 SSA2                
112200     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
112300     PERFORM IMS-STATUSKONTROLL                                           
112400     .                                                                    
112500                                                                          
112600 IMS-GET-XXKF-RADER-LAST SECTION.                                         
112700                                                                          
112800     MOVE 'WLXXKF11*L ' TO SSA1                                           
112900     MOVE '  GE' TO GODK-STATUSKODER                                      
113000     CALL CBLTDLI USING GNP XXKF-PCB DLI-IO-AREA SSA1                     
113100     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
113200     PERFORM IMS-STATUSKONTROLL                                           
113300     .                                                                    
113400                                                                          
113500                                                                          
113600 IMS-ISRT-XXKF-RADER SECTION.                                             
113700                                                                          
113800     STRING 'WLXXKF01(WDGXKEY  =' W-IDHTYP-X ')'                          
113900          DELIMITED BY SIZE INTO SSA1                                     
114000     MOVE 'WLXXKF11 ' TO SSA2                                             
114100     MOVE '  II' TO GODK-STATUSKODER                                      
114200     CALL CBLTDLI USING ISRT XXKF-PCB DLI-IO-AREA SSA1 SSA2               
114300     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
114400     PERFORM IMS-STATUSKONTROLL                                           
114500     .                                                                    
114600                                                                          
114700 IMS-ISRT-XXKF-TABELL SECTION.                                            
114800                                                                          
114900     MOVE 'WLXXKF01 ' TO SSA1                                             
115000     MOVE '  II' TO GODK-STATUSKODER                                      
115100     CALL CBLTDLI USING ISRT XXKF-PCB DLI-IO-AREA SSA1                    
115200     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
115300     PERFORM IMS-STATUSKONTROLL                                           
115400     .                                                                    
115500                                                                          
115600 IMS-DLET-XXKF-RADER SECTION.                                             
115700                                                                          
115800     MOVE '  ' TO GODK-STATUSKODER                                        
115900     CALL CBLTDLI USING DLET XXKF-PCB DLI-IO-AREA                         
116000     MOVE XXKF-STATUS-CODE TO STATUS-WS                                   
116100     PERFORM IMS-STATUSKONTROLL                                           
116200     .                                                                    
116300     EJECT                                                                
116310 IMS-GU-WDB601    SECTION.                                                
116320     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
116330          DELIMITED BY SIZE INTO SSA1                                     
116340     MOVE '  GE' TO GODK-STATUSKODER                                      
116350     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
116360     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
116370     PERFORM IMS-STATUSKONTROLL                                           
116380     IF SEGMENT-SAKNAS                                                    
116390         MOVE SPACE TO DCS-KDDC                                           
116391     END-IF                                                               
116392     .                                                                    
116400 IMS-STATUSKONTROLL SECTION.                                              
116500                                                                          
116600     SET STATUS-IX TO 1                                                   
116700     SEARCH GODK-STATUS                                                   
116800       AT END CALL FELLOG                                                 
116900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
117000     END-SEARCH                                                           
117100     .                                                                    
