000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4036200.                                                
000400 AUTHOR.         GERRY CARMICHAEL.                                        
000500 DATE-WRITTEN.   FEB.  90.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        UPPDATERING, NYUPPLÄGGNING OCH FRÅGEPROGRAM.                     
001100*                                                                         
001200*        UNDERHÅLL HLO-TABELL.                                            
001300*        PROGRAMMET VISAR OCH GÖR FÖRÄNDRINGAR MOT                        
001400*        DEN TABELL SOM VISAR VILKA FYSISKA LAGER-                        
001500*        OMRÅDEN SOM SKALL PRODUCERAS IHOP I SAMMA                        
001600*        PLOCKSATSER.                                                     
001700*                                                                         
001800*        PROGRAMMET LÄSER OCH UPPDATERAR WDR1.                            
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W4T362                                              
002200*                     W4T362U                                             
002300*        MID:         W4I36201                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W4O36201                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                   PIC X(8)    VALUE 'W4036200'.                
003400 77  JA                      PIC X       VALUE 'J'.                       
003500 77  NEJ                     PIC X       VALUE 'N'.                       
003600 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
003700 77  INDX                    PIC S9(9)   VALUE +1   COMP SYNC.            
003800 77  IX                      PIC S9(9)   VALUE +0   COMP SYNC.            
003900 77  RAD-INDX                PIC S9(9)   VALUE +0   COMP SYNC.            
004000 77  MAX-IX                  PIC S9(9)   VALUE +5   COMP SYNC.            
004100 77  MAX-IDHLO               PIC S9(9)   VALUE +99  COMP SYNC.            
004200 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +949 COMP SYNC.            
004300 77  IDHLOTAB-WS             PIC  X(2)   VALUE SPACE.                     
004400 77  IDHLO-WS                PIC  9(2)   VALUE ZERO.                      
004530       EJECT                                                              
004600 01  TABELL.                                                              
004700     03  SPAR-TABELL OCCURS 5.                                            
004800        05  SPARRAD          PIC S9(2)   VALUE ZERO.                      
004900                                                                          
005000 77  INDATA-SW               PIC X       VALUE 'J'.                       
005100   88  INDATA-OK                         VALUE 'J'.                       
005200   88  INDATA-FEL                        VALUE 'N'.                       
005300                                                                          
005400 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
005500   88  NYCKLAR-OK                        VALUE 'J'.                       
005600   88  NYCKLAR-FEL                       VALUE 'N'.                       
005700                                                                          
005800 77  IDHLO-SW                PIC X       VALUE 'J'.                       
005900   88  IDHLO-IFYLLT                      VALUE 'J'.                       
006000                                                                          
006100 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
006200   88  EGEN-TRANS                        VALUE '4362'.                    
006300   88  GODK-TRANS                        VALUE '4361' '4362'              
006400                                               '4363' '4366'.             
006500                                                                          
006600 01  GENERELLA-SUBPROGRAM.                                                
006700   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
006800   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
006900   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
006910   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
006920     EJECT                                                                
006930*   -COPY WDECAREA                                                        
006940     EJECT                                                                
006950*   -COPY WMEDAREA                                                        
006960     EJECT                                                                
006970*                   ****    PARAMETRAR TILL W005INIT                      
006980*01  -COPY WMSGINIT                                                       
006990     EJECT                                                                
007700******************************************************************        
007800*                                                                         
007900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
008000*                                                                         
008100 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
008200     SKIP3                                                                
008300*01  MID -COPY W4I36201                                                   
008500     EJECT                                                                
008600*01  -COPY WMSGAREA                                                       
008800     EJECT                                                                
008900*  03  MOD -COPY W4O36201 -RED MSG-AREA.                                  
009100     EJECT                                                                
009200*01  -COPY WMFSAREA                                                       
009400     EJECT                                                                
009500******************************************************************        
009600*                                                                         
009700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009800*                                                                         
009900 01  IMS-WS.                                                              
010000   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
010100     SKIP3                                                                
010200*                        **** STATUS-KOD FRÅN IMS                         
010300   03  STATUS-WS             PIC XX.                                      
010400     88  SEGMENT-FINNS                   VALUE '  '.                      
010500     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
010600     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
010700     88  SEGMENT-HOGRE                   VALUE 'GA'.                      
010800     SKIP3                                                                
010900   03  GODK-STATUSKODER.                                                  
011000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011100     SKIP3                                                                
011200 01  NYCKLAR-TILL-DLI.                                                    
011300                                                                          
011400   03  W-4441-HTYP-X.                                                     
011500     05  W-HTYP              PIC  9(4)   VALUE 4441.                      
011600     05  FILLER              PIC  X(26)  VALUE LOW-VALUE.                 
011700                                                                          
011800   03  W-IDHLOTAB-X.                                                      
011900     05  W-4442-IDDC             PIC  X(2)   VALUE '00'.                  
012000     05  W-4442-IDHLOTAB         PIC  9(2)   VALUE ZERO.                  
012100     05  W-4442-LOW-VALUE        PIC  X(6)   VALUE LOW-VALUE.             
012200                                                                          
012300   03  W-IDHLOTAB-DEF-X.                                                  
012400     05  W-4442-IDDC-DEF         PIC  X(2)   VALUE '00'.                  
012500     05  W-4442-IDHLOTAB-DEF     PIC  9(2)   VALUE 01.                    
012600     05  W-4442-LOW-VALUE-DEF    PIC  X(6)   VALUE LOW-VALUE.             
012610                                                                          
012620   03  W-IDDC-B6-X.                                                       
012630       05 W-IDDC-B6                  PIC X(2).                            
012640                                                                          
012700     SKIP3                                                                
012800 01    SSA1                  PIC X(64).                                   
012900 01    SSA2                  PIC X(64).                                   
013000     EJECT                                                                
013100*                            IMS FUNKTIONSKODER                           
013200*01    -COPY W0003                                                        
013400     EJECT                                                                
013500*                            DLI INPUT-OUTPUT AREA                        
013600 01  DLI-IO-AREA.                                                         
013700   03  IO-AREA               PIC X(300)  VALUE SPACE.                     
013800     SKIP3                                                                
013900*  03  WLXXKE01  -COPY WDGX4442  -RED IO-AREA.                            
014000                                                                          
014010 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014020 01   DLI-IO-AREA-B601.                                                   
014030*     03  -COPY WDB601                                                    
014040                                                                          
014100     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014300*01  -COPY W0009     -PRE MSG-                                            
014400     EJECT                                                                
014410*01  -COPY W0008     -PRE USEA-                                           
014420     05  FILLER              PIC X.                                       
014500     EJECT                                                                
014600*01  -COPY W0008     -PRE XXKE-                                           
014800     05  FILLER              PIC X.                                       
014900     EJECT                                                                
014910*01  -COPY W0008     -PRE WDB6-                                           
014920     05  FILLER              PIC X.                                       
014930     EJECT                                                                
015000 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
015010                                  XXKE-PCB WDB6-PCB.                      
015100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
015110                                   XXKE-PCB WDB6-PCB.                     
015200                                                                          
015300     PERFORM IMS-GET-MSG                                                  
015400     IF SEGMENT-FINNS                                                     
015500       PERFORM A-INIT                                                     
015600       PERFORM B-KOLLA-NYCKLAR                                            
015700       IF NYCKLAR-OK                                                      
015800         IF MFS-UPDATE                                                    
015900           PERFORM G-KOLLA-INPUT                                          
016000           IF INDATA-OK                                                   
016100             PERFORM H-UPPDATERA                                          
016200           END-IF                                                         
016300         ELSE                                                             
016400           PERFORM C-BEHANDLA-HLOTABELL                                   
016500         END-IF                                                           
016600       END-IF                                                             
016700       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
016800       PERFORM IMS-INSERT-MSG                                             
016900     END-IF                                                               
017000                                                                          
017100     MOVE ZERO TO RETURN-CODE                                             
017200     GOBACK                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 A-INIT SECTION.                                                          
017600                                                                          
017700     IF MSG-DUBBLA-TRANSKODER                                             
017800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I36201                 
017900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
018000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018100     ELSE                                                                 
018200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I36201                  
018300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
018400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018500     END-IF                                                               
018600                                                                          
018700     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
018800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
018900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019000     MOVE LOW-VALUE TO MSG-AREA                                           
019100     MOVE 'W4O362N1' TO MFS-IDMOD                                         
019200     MOVE '4362' TO MOD-IDTRANS                                           
019300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019400                                                                          
019500     IF NOT EGEN-TRANS                                                    
019600       MOVE SPACE TO MFS-KDTRTYP                                          
019700       MOVE '7' TO MFS-IDPFK                                              
019800     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021300 B-KOLLA-NYCKLAR SECTION.                                                 
021310                                                                          
021320     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021330     MOVE '001'             TO MSGI-KDCALL                                
021340     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021341     MOVE '4362'            TO MSGI-IDTRANS                               
021342     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021350     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021360                                                                          
021370     IF MSGI-IDLAND-SPR = 'GB'                                            
021380       MOVE +2 TO SPRAK-IX                                                
021390       MOVE 'GB ' TO MED-IDSKYLT                                          
021391     ELSE                                                                 
021392       MOVE +1 TO SPRAK-IX                                                
021393       MOVE 'S  ' TO MED-IDSKYLT                                          
021394     END-IF                                                               
021400                                                                          
021500     MOVE NEJ TO IDHLO-SW                                                 
021600     MOVE JA TO NYCKLAR-SW                                                
021700     MOVE LOW-VALUE TO 4442-LOW-VALUE                                     
021800     MOVE MFS-RENSA-FAELT TO MOD-IDHLOTAB-IN                              
021900                             MOD-IDHLO-IN                                 
022000                                                                          
022100     IF MID-IDHLOTAB-IN = ALL '+'                                         
022200       MOVE MID-IDHLOTAB-UT TO IDHLOTAB-WS                                
022300       INSPECT IDHLOTAB-WS REPLACING LEADING SPACE BY ZERO                
022400     ELSE                                                                 
022500       MOVE MID-IDHLOTAB-IN TO IDHLOTAB-WS                                
022600       INSPECT IDHLOTAB-WS REPLACING LEADING SPACE BY ZERO                
022700       MOVE '7'            TO MFS-IDPFK                                   
022800       MOVE SPACE          TO MFS-KDTRTYP                                 
022900     END-IF                                                               
023000                                                                          
023100     IF MID-IDHLO-IN = ALL '+'                                            
023200       MOVE MID-IDHLO-UT TO IDHLO-WS                                      
023300       INSPECT IDHLO-WS REPLACING LEADING SPACE BY ZERO                   
023400     ELSE                                                                 
023500       MOVE MID-IDHLO-IN TO IDHLO-WS                                      
023600       INSPECT IDHLO-WS REPLACING LEADING SPACE BY ZERO                   
023700       MOVE '7'            TO MFS-IDPFK                                   
023800       MOVE SPACE          TO MFS-KDTRTYP                                 
023900     END-IF                                                               
023910                                                                          
023920     MOVE MSGI-IDDC               TO W-IDDC-B6                            
023930     PERFORM IMS-GU-WDB601                                                
024000                                                                          
024105     IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                         
024106       MOVE NEJ                     TO NYCKLAR-SW                         
024107     ELSE                                                                 
024108       MOVE DCS-IDDC                TO W-4442-IDDC                        
024109                                       W-4442-IDDC-DEF                    
024113     END-IF                                                               
024114                                                                          
024120     PERFORM BA-NUMERISK-KONTROLL                                         
024200                                                                          
024300     IF GODK-TRANS OR NYCKLAR-OK                                          
024400       MOVE IDHLOTAB-WS TO MOD-IDHLOTAB-UT                                
024500       INSPECT MOD-IDHLOTAB-UT REPLACING LEADING ZERO BY SPACE            
024600       IF IDHLO-WS NOT = ZERO                                             
024700         MOVE IDHLO-WS TO MOD-IDHLO-UT                                    
024800         INSPECT MOD-IDHLO-UT REPLACING LEADING ZERO BY SPACE             
024900         MOVE JA TO IDHLO-SW                                              
025000       ELSE                                                               
025100         MOVE MFS-RENSA-FAELT TO MOD-IDHLO-UT                             
025200         MOVE NEJ TO IDHLO-SW                                             
025300       END-IF                                                             
025301                                                                          
025330       IF NYCKLAR-OK                                                      
025392         MOVE DCS-IDDC TO MOD-IDDC-UT                                     
025395         INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE              
025396       END-IF                                                             
025400     ELSE                                                                 
025500       MOVE MFS-RENSA-FAELT TO MOD-IDHLOTAB-UT                            
025600                               MOD-IDHLO-UT                               
025610                               MOD-IDDC-UT                                
025700     END-IF                                                               
025800     IF NYCKLAR-FEL                                                       
025900       PERFORM MFS-RENSA-FAELT-IN                                         
026000       PERFORM MFS-RENSA-FAELT-UT                                         
026100       IF NOT GODK-TRANS                                                  
026200         MOVE MFS-RENSA-FAELT TO MOD-IDHLO-IN                             
026300                                 MOD-IDHLO-UT                             
026400                                 MOD-IDHLOTAB-IN                          
026500                                 MOD-IDHLOTAB-UT                          
026510                                 MOD-IDDC-IN                              
026520                                 MOD-IDDC-UT                              
026600       END-IF                                                             
026700     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000                                                                          
027100 BA-NUMERISK-KONTROLL SECTION.                                            
027200                                                                          
027210     IF IDHLO-WS NOT NUMERIC                                              
027220        MOVE ZERO TO IDHLO-WS                                             
027230     END-IF                                                               
027240                                                                          
027250     IF IDHLOTAB-WS NOT NUMERIC                                           
027260        MOVE ZERO TO IDHLOTAB-WS                                          
027270     END-IF                                                               
027280                                                                          
030000     MOVE IDHLOTAB-WS TO W-4442-IDHLOTAB                                  
030100                                                                          
030200     IF IDHLO-WS NUMERIC AND IDHLO-WS NOT = ZERO                          
030300        MOVE IDHLO-WS TO 4442-IDHLO(INDX)                                 
030400     END-IF                                                               
030600     .                                                                    
030700     EJECT                                                                
030900 C-BEHANDLA-HLOTABELL SECTION.                                            
031000                                                                          
031100     PERFORM IMS-GHU-XXKE-XXKE11                                          
031200     IF SEGMENT-FINNS                                                     
031300       IF NOT IDHLO-IFYLLT                                                
031400         PERFORM S01-LAES-VISA-HLO-RAD                                    
031500       ELSE                                                               
031600         PERFORM F-LAES-VISA-VISSA-HLO                                    
031700       END-IF                                                             
031800       IF MID-INPUT NOT = ALL '+'                                         
031810          AND                                                             
031820          MFS-ENTER                                                       
031900*************   'TRYCK PF11 FÖR UPPDATERING'*********************         
032000         MOVE '407' TO MED-IDMFSFEL                                       
032100         CALL WMEDKONV USING MED-WMEDAREA                                 
032200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
032300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
032400         PERFORM MFS-LAES-IN-IGEN                                         
032500       END-IF                                                             
032600     ELSE                                                                 
032700*********   'TABELL SAKNAS'**************************************         
032800       MOVE '023' TO MED-IDMFSFEL                                         
032900       CALL WMEDKONV USING MED-WMEDAREA                                   
033000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
033100       MOVE MFS-RENSA-FAELT TO MOD-IDHLO-UT                               
033200       PERFORM MFS-RENSA-FAELT-IN                                         
033300       MOVE NEJ TO NYCKLAR-SW                                             
033400     END-IF                                                               
033500     .                                                                    
033600     EJECT                                                                
033700 F-LAES-VISA-VISSA-HLO SECTION.                                           
033800                                                                          
033900     MOVE +1 TO INDX                                                      
034000     PERFORM UNTIL INDX > MAX-IDHLO                                       
034100       MOVE INDX TO MOD-ADLAGOMR-RAD(INDX)                                
034200       ADD +1 TO INDX                                                     
034300     END-PERFORM                                                          
034400                                                                          
034500     MOVE +1 TO INDX                                                      
034600     PERFORM UNTIL INDX > MAX-IDHLO                                       
034700       IF SEGMENT-FINNS                                                   
034800         IF 4442-IDHLO(INDX) = IDHLO-WS                                   
034900           MOVE 4442-IDHLO(INDX) TO MOD-IDHLO-RAD(INDX)                   
035000         ELSE                                                             
035100           MOVE MFS-RENSA-FAELT TO MOD-IDHLO-RAD(INDX)                    
035200         END-IF                                                           
035300       END-IF                                                             
035400       ADD +1 TO INDX                                                     
035500     END-PERFORM                                                          
035600                                                                          
035700     PERFORM MFS-RENSA-FAELT-IN                                           
035800     .                                                                    
035900     EJECT                                                                
036000 G-KOLLA-INPUT SECTION.                                                   
036100                                                                          
036200     MOVE +1 TO IX                                                        
036300     MOVE JA TO INDATA-SW                                                 
036400                                                                          
036500     PERFORM UNTIL IX > MAX-IX                                            
036600       IF MID-ADLAGOMR-UPDATE(IX) NOT = ALL '+' OR                        
036700         MID-IDHLO-UPDATE(IX) NOT = ALL '+'                               
036800                                                                          
036900         IF MID-ADLAGOMR-UPDATE(IX) NOT = ALL '+'                         
037000           IF MID-ADLAGOMR-UPDATE(IX) NOT NUMERIC OR                      
037100             MID-ADLAGOMR-UPDATE(IX) < +1                                 
037200             MOVE MFS-NUM-FAELT-FEL TO                                    
037300                                  MOD-ADLAGOMR-UPDATE-ATTR(IX)            
037400             MOVE NEJ TO INDATA-SW                                        
037500           ELSE                                                           
037600            MOVE MFS-NUM-FAELT-RAETT TO                                   
037700                                  MOD-ADLAGOMR-UPDATE-ATTR(IX)            
037800           END-IF                                                         
037900         END-IF                                                           
038000                                                                          
038100         IF MID-IDHLO-UPDATE(IX) = ALL '+'                                
038103           MOVE 99               TO MID-IDHLO-UPDATE(IX)                  
038110         ELSE                                                             
038200           IF MID-IDHLO-UPDATE(IX) NOT NUMERIC OR                         
038300             MID-IDHLO-UPDATE(IX) < +1                                    
038400             MOVE MFS-NUM-FAELT-FEL TO MOD-IDHLO-UPDATE-ATTR(IX)          
038500             MOVE NEJ TO INDATA-SW                                        
038600           ELSE                                                           
038700             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDHLO-UPDATE-ATTR(IX)        
038800           END-IF                                                         
038900         END-IF                                                           
039000                                                                          
039100         IF MID-ADLAGOMR-UPDATE(IX) = ALL '+' AND                         
039200           MID-IDHLO-UPDATE(IX) NOT = ALL '+'                             
039300           MOVE MFS-NUM-FAELT-FEL TO MOD-ADLAGOMR-UPDATE-ATTR(IX)         
039400           MOVE NEJ TO INDATA-SW                                          
039500         END-IF                                                           
039600                                                                          
039700         IF MID-ADLAGOMR-UPDATE(IX) NOT = ALL '+' AND                     
039800           MID-IDHLO-UPDATE(IX) = ALL '+'                                 
039900           MOVE MFS-NUM-FAELT-FEL TO MOD-IDHLO-UPDATE-ATTR(IX)            
040000           MOVE NEJ TO INDATA-SW                                          
040100         END-IF                                                           
040200         ADD +1 TO IX                                                     
040300       ELSE                                                               
040400        ADD +1 TO IX                                                      
040500       END-IF                                                             
040600     END-PERFORM                                                          
040700                                                                          
040800     IF INDATA-FEL                                                        
040900****************'KORRIGERA UPPLYSTA FÄLT'*********************            
041000       MOVE '001' TO MED-IDMFSFEL                                         
041100       CALL WMEDKONV USING MED-WMEDAREA                                   
041200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
041300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
041400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
041500     END-IF                                                               
041600     .                                                                    
041700     EJECT                                                                
041800 H-UPPDATERA SECTION.                                                     
041900                                                                          
042000     PERFORM IMS-GHU-XXKE-XXKE11                                          
042100     IF SEGMENT-FINNS                                                     
042200       PERFORM HA-TABELL-AENDRING                                         
042300     ELSE                                                                 
042400       PERFORM HB-TABELL-NYUPPLAEGG                                       
042500     END-IF                                                               
042600     .                                                                    
042700     EJECT                                                                
042800                                                                          
042900 HA-TABELL-AENDRING SECTION.                                              
043000                                                                          
043100     MOVE JA TO INDATA-SW                                                 
043200     MOVE +1 TO IX                                                        
043300     IF 4442-IDHLOTAB = 01 OR                                             
043400      (4442-IDHLOTAB = 02 AND SPRAK-IX = +1)                              
043500***********   'UPPDATERING EJ TILLÅTEN'**************************         
043600       MOVE '777' TO MED-IDMFSFEL                                         
043700       CALL WMEDKONV USING MED-WMEDAREA                                   
043800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
043900       PERFORM MFS-RENSA-FAELT-IN                                         
044000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
044100     ELSE                                                                 
044200       IF MID-INPUT NOT = ALL '+'                                         
044300         PERFORM UNTIL IX > MAX-IX                                        
044400           IF MID-ADLAGOMR-UPDATE(IX) NOT = ALL '+' AND                   
044500             MID-IDHLO-UPDATE(IX) NOT = ALL '+'                           
044600             MOVE MID-ADLAGOMR-UPDATE(IX) TO INDX                         
044700             IF IDHLO-IFYLLT                                              
044800               IF 4442-IDHLO(INDX) = IDHLO-WS                             
044900                 MOVE MID-ADLAGOMR-UPDATE(IX) TO SPARRAD(IX)              
045000                 MOVE MID-IDHLO-UPDATE(IX) TO 4442-IDHLO(INDX)            
045100               ELSE                                                       
045200                 MOVE MFS-NUM-FAELT-FEL TO                                
045300                                  MOD-ADLAGOMR-UPDATE-ATTR(IX)            
045400                 MOVE MFS-NUM-FAELT-FEL TO                                
045500                                  MOD-IDHLO-UPDATE-ATTR(IX)               
045600                 MOVE NEJ TO INDATA-SW                                    
045700               END-IF                                                     
045800             ELSE                                                         
045900               MOVE MID-ADLAGOMR-UPDATE(IX) TO SPARRAD(IX)                
046000               MOVE MID-IDHLO-UPDATE(IX) TO 4442-IDHLO(INDX)              
046100             END-IF                                                       
046200             ADD +1 TO IX                                                 
046300           ELSE                                                           
046400             ADD +1 TO IX                                                 
046500           END-IF                                                         
046600         END-PERFORM                                                      
046700         IF INDATA-OK                                                     
046800           PERFORM IMS-REPL-WLXXKE11                                      
046900           PERFORM S01-LAES-VISA-HLO-RAD                                  
047000           MOVE MFS-RENSA-FAELT TO MOD-IDHLO-UT                           
047100           PERFORM MFS-FORM-ATTR                                          
047200           PERFORM MFS-RENSA-FAELT-IN                                     
047300         ELSE                                                             
047400***********   'UPPDATERING EJ TILLÅTEN'**************************         
047500           MOVE '777' TO MED-IDMFSFEL                                     
047600           CALL WMEDKONV USING MED-WMEDAREA                               
047700           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
047800           PERFORM MFS-ROER-EJ-FAELT-IN                                   
047900           PERFORM MFS-ROER-EJ-FAELT-UT                                   
048000         END-IF                                                           
048100       ELSE                                                               
048200************* 'PF11 OCH TOM INDATARAD'*************************           
048300         MOVE '011' TO MED-IDMFSFEL                                       
048400         CALL WMEDKONV USING MED-WMEDAREA                                 
048500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
048600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
048700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
048800       END-IF                                                             
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 HB-TABELL-NYUPPLAEGG SECTION.                                            
049300     IF MID-IDHLOTAB-UT NOT = SPACE                                       
049400       MOVE +1 TO INDX                                                    
049500       PERFORM IMS-GHU-XXKE-XXKE11-DEF                                    
049600       IF SEGMENT-FINNS                                                   
049700         PERFORM UNTIL INDX > MAX-IDHLO                                   
049800           MOVE 4442-IDHLO(INDX) TO MOD-IDHLO-RAD(INDX)                   
049900           ADD +1 TO INDX                                                 
050000         END-PERFORM                                                      
050100         MOVE +1 TO INDX                                                  
050200         PERFORM UNTIL INDX > MAX-IDHLO                                   
050300           MOVE INDX TO MOD-ADLAGOMR-RAD(INDX)                            
050400           ADD +1 TO INDX                                                 
050500         END-PERFORM                                                      
050600         MOVE IDHLOTAB-WS TO 4442-IDHLOTAB                                
050700         PERFORM IMS-ISRT-WLXXKE11                                        
050800************   'GENERELL TABELL UPPLAGD'*************************         
050900         MOVE '024' TO MED-IDMFSINF                                       
051000         CALL WMEDKONV USING MED-WMEDAREA                                 
051100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
051200         MOVE MFS-RENSA-FAELT TO MOD-IDHLO-UT                             
051300         IF MID-INPUT NOT = ALL '+'                                       
051400           PERFORM MFS-ROER-EJ-FAELT-IN                                   
051500         ELSE                                                             
051600           PERFORM MFS-FORM-ATTR                                          
051700           PERFORM MFS-RENSA-FAELT-IN                                     
051800         END-IF                                                           
051900       END-IF                                                             
052000     END-IF                                                               
052100     .                                                                    
052200     EJECT                                                                
052300 S01-LAES-VISA-HLO-RAD SECTION.                                           
052400     MOVE +1 TO INDX                                                      
052500     PERFORM UNTIL INDX > MAX-IDHLO                                       
052600       MOVE INDX TO MOD-ADLAGOMR-RAD(INDX)                                
052700       ADD +1 TO INDX                                                     
052800     END-PERFORM                                                          
052900                                                                          
053000     MOVE +1 TO INDX                                                      
053100     PERFORM UNTIL INDX > MAX-IDHLO                                       
053200       IF SEGMENT-FINNS                                                   
053300          IF 4442-IDHLO(INDX) = 99                                        
053310             MOVE MFS-RENSA-FAELT  TO MOD-IDHLO-RAD(INDX)                 
053320          ELSE                                                            
053330             MOVE 4442-IDHLO(INDX) TO MOD-IDHLO-RAD(INDX)                 
053340          END-IF                                                          
053400       END-IF                                                             
053500       ADD +1 TO INDX                                                     
053600     END-PERFORM                                                          
053700                                                                          
053800     MOVE +1 TO IX                                                        
053900     PERFORM UNTIL IX > MAX-IX                                            
054000       IF SPARRAD(IX) NOT = ZERO                                          
054100         MOVE SPARRAD(IX) TO RAD-INDX                                     
054200         MOVE MFS-ADD-LYS-UPP-FAELT TO                                    
054300                             MOD-IDHLO-RAD-ATTR(RAD-INDX)                 
054400         MOVE MFS-ADD-LYS-UPP-FAELT TO                                    
054500                             MOD-ADLAGOMR-RAD-ATTR(RAD-INDX)              
054600********      'UPPDATERING UTFÖRD'********************************        
054700         MOVE '404' TO MED-IDMFSINF                                       
054800         CALL WMEDKONV USING MED-WMEDAREA                                 
054900         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
055000         MOVE ZERO TO SPARRAD(IX)                                         
055100       END-IF                                                             
055200       ADD +1 TO IX                                                       
055300     END-PERFORM                                                          
055400     PERFORM MFS-RENSA-FAELT-IN                                           
055500     .                                                                    
055600     EJECT                                                                
055700                                                                          
055800 MFS-RENSA-FAELT-UT SECTION.                                              
055900                                                                          
056000     MOVE +1 TO INDX                                                      
056100     PERFORM UNTIL INDX > MAX-IDHLO                                       
056200       MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-RAD(INDX)                     
056300       ADD +1 TO INDX                                                     
056400     END-PERFORM                                                          
056500     MOVE +1 TO INDX                                                      
056600     PERFORM UNTIL INDX > MAX-IDHLO                                       
056700       MOVE MFS-RENSA-FAELT TO MOD-IDHLO-RAD(INDX)                        
056800       ADD +1 TO INDX                                                     
056900     END-PERFORM                                                          
057000     .                                                                    
057100     SKIP2                                                                
057200 MFS-RENSA-FAELT-IN SECTION.                                              
057300                                                                          
057400     MOVE +1 TO IX                                                        
057500     PERFORM UNTIL IX > MAX-IX                                            
057600       MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-UPDATE(IX)                    
057700                               MOD-IDHLO-UPDATE(IX)                       
057800       ADD +1 TO IX                                                       
057900     END-PERFORM                                                          
058000     .                                                                    
058100     EJECT                                                                
058200 MFS-ROER-EJ-FAELT-UT SECTION.                                            
058300     MOVE +1 TO INDX                                                      
058400     PERFORM UNTIL INDX > MAX-IDHLO                                       
058500       MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR-RAD(INDX)                   
058600       ADD +1 TO INDX                                                     
058700     END-PERFORM                                                          
058800     MOVE +1 TO INDX                                                      
058900     PERFORM UNTIL INDX > MAX-IDHLO                                       
059000       MOVE MFS-ROER-EJ-FAELT TO MOD-IDHLO-RAD(INDX)                      
059100       ADD +1 TO INDX                                                     
059200     END-PERFORM                                                          
059300     .                                                                    
059400     SKIP2                                                                
059500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
059600     MOVE +1 TO IX                                                        
059700     PERFORM UNTIL IX > MAX-IX                                            
059800       MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR-UPDATE(IX)                  
059900                                 MOD-IDHLO-UPDATE(IX)                     
060000       ADD +1 TO IX                                                       
060100     END-PERFORM                                                          
060200     .                                                                    
060300     EJECT                                                                
060400 MFS-FORM-ATTR SECTION.                                                   
060500                                                                          
060600     MOVE +1 TO IX                                                        
060700     PERFORM UNTIL IX > MAX-IX                                            
060800       MOVE MFS-FORMATETS-ATTR TO MOD-ADLAGOMR-UPDATE-ATTR(IX)            
060900                                  MOD-IDHLO-UPDATE-ATTR(IX)               
061000       ADD +1 TO IX                                                       
061100     END-PERFORM                                                          
061200     .                                                                    
061300     SKIP2                                                                
061400 MFS-LAES-IN-IGEN SECTION.                                                
061500     MOVE +1 TO IX                                                        
061600     PERFORM UNTIL IX > MAX-IX                                            
061700      IF MID-ADLAGOMR-UPDATE(IX) NOT = ALL '+'                            
061800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLAGOMR-UPDATE-ATTR(IX)        
061900      END-IF                                                              
062000      IF MID-IDHLO-UPDATE(IX) NOT = ALL '+'                               
062100        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDHLO-UPDATE-ATTR(IX)           
062200      END-IF                                                              
062300      ADD +1 TO IX                                                        
062400     END-PERFORM                                                          
062500     .                                                                    
062600     EJECT                                                                
062700* IMS SEKTIONER                                                           
062800     SKIP3                                                                
062900 IMS-GET-MSG SECTION.                                                     
063000                                                                          
063100     MOVE '  QC' TO GODK-STATUSKODER                                      
063200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
063300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063400     PERFORM IMS-STATUSKONTROLL                                           
063500     .                                                                    
063600     SKIP3                                                                
063700 IMS-INSERT-MSG SECTION.                                                  
063800                                                                          
063810     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
064000       MOVE '0' TO MFS-KDHUVOMR                                           
064100     END-IF                                                               
064200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
064300     MOVE SPACE TO GODK-STATUSKODER                                       
064400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
064500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
064600     PERFORM IMS-STATUSKONTROLL                                           
064700     .                                                                    
064800     EJECT                                                                
064900                                                                          
065000 IMS-GHU-XXKE-XXKE11-DEF SECTION.                                         
065100                                                                          
065200     STRING 'WLXXKE01(WDGXKEY  =' W-4441-HTYP-X ')'                       
065300          DELIMITED BY SIZE INTO SSA1                                     
065400     STRING 'WLXXKE11(WDGXKEY  =' W-IDHLOTAB-DEF-X ')'                    
065500          DELIMITED BY SIZE INTO SSA2                                     
065600     MOVE '  GE' TO GODK-STATUSKODER                                      
065700     CALL CBLTDLI USING GHU XXKE-PCB DLI-IO-AREA SSA1 SSA2                
065800     MOVE XXKE-STATUS-CODE TO STATUS-WS                                   
065900     PERFORM IMS-STATUSKONTROLL                                           
066000     .                                                                    
066100     EJECT                                                                
066200                                                                          
066300 IMS-GHU-XXKE-XXKE11 SECTION.                                             
066400                                                                          
066500     STRING 'WLXXKE01(WDGXKEY  =' W-4441-HTYP-X ')'                       
066600            DELIMITED BY SIZE INTO SSA1                                   
066700     STRING 'WLXXKE11(WDGXKEY  =' W-IDHLOTAB-X ')'                        
066800          DELIMITED BY SIZE INTO SSA2                                     
066900     MOVE '  GE' TO GODK-STATUSKODER                                      
067000     CALL CBLTDLI USING GHU XXKE-PCB DLI-IO-AREA SSA1 SSA2                
067100     MOVE XXKE-STATUS-CODE TO STATUS-WS                                   
067200     PERFORM IMS-STATUSKONTROLL                                           
067300     .                                                                    
067400     EJECT                                                                
067500                                                                          
067600 IMS-ISRT-WLXXKE11 SECTION.                                               
067700                                                                          
067800     STRING 'WLXXKE01(WDGXKEY  =' W-4441-HTYP-X ')'                       
067900            DELIMITED BY SIZE INTO SSA1                                   
068000     MOVE 'WLXXKE11 ' TO SSA2                                             
068100     MOVE '  II' TO GODK-STATUSKODER                                      
068200     CALL CBLTDLI USING ISRT XXKE-PCB DLI-IO-AREA SSA1 SSA2               
068300     MOVE XXKE-STATUS-CODE TO STATUS-WS                                   
068400     PERFORM IMS-STATUSKONTROLL                                           
068500     .                                                                    
068600     EJECT                                                                
068700 IMS-REPL-WLXXKE11 SECTION.                                               
068800                                                                          
068900     MOVE '  ' TO GODK-STATUSKODER                                        
069000     CALL CBLTDLI USING REPL XXKE-PCB DLI-IO-AREA                         
069100     MOVE XXKE-STATUS-CODE TO STATUS-WS                                   
069200     PERFORM IMS-STATUSKONTROLL                                           
069300     .                                                                    
069400                                                                          
069410 IMS-GU-WDB601    SECTION.                                                
069420     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
069430          DELIMITED BY SIZE INTO SSA1                                     
069440     MOVE '  GE' TO GODK-STATUSKODER                                      
069450     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
069460     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
069470     PERFORM IMS-STATUSKONTROLL                                           
069480     IF SEGMENT-SAKNAS                                                    
069490         MOVE SPACE TO DCS-KDDC                                           
069491     END-IF                                                               
069492     .                                                                    
069500 IMS-STATUSKONTROLL SECTION.                                              
069600                                                                          
069700     SET STATUS-IX TO 1                                                   
069800     SEARCH GODK-STATUS                                                   
069900       AT END CALL FELLOG                                                 
070000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
070100     END-SEARCH                                                           
070200     .                                                                    
