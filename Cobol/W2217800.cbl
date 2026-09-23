000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2217800.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   09/11/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        READ THE SUPERCEEDED PART NUMBERS AND ACCORDINGLY                
001000*        UDPATE THE VALUES TO THE SUPERCEEDING PART NUMBERS               
001100*                                                                         
001200*        THE PROGRAM READS     WDD7                                       
001300*        THE PROGRAM UPDATES   WDK6                                       
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- INPUT FILE FROM W2217700                                   
002400     SELECT W22177                     ASSIGN TO W22178D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W22177                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY W2217701      -L.                                              
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W2217800'.            
003900 01  CHKP-VAR.                                                            
004000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004500     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004804 77  W-DAREGDAT                  PIC 9(08)   VALUE ZERO.                  
005004 77  W-TID                       PIC 9(08)   VALUE ZERO.                  
005200     SKIP2                                                                
005210 01  WORKING-AREA.                                                        
005220     03  WS-KVULOAD              PIC 9(7)    VALUE ZERO.                  
005230                                                                          
005300 01  ERROR-TEXT.                                                          
005400     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005600                                                                          
005700 77  W22177-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W22177                       VALUE 'Y'.                   
005900     EJECT                                                                
006000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES TODAYS-DATE.                                        
006200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006400     03  TODAYS-DATE-DAY         PIC 9(2).                                
006500     EJECT                                                                
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL POSTSUM                                          
007300*                                                                         
007400*01  -COPY W0005   -PRE  POSTSUM-                                         
007500     EJECT                                                                
007600 01  W22177-AREA-START           PIC X(24)   VALUE                        
007700                                             'W22177-AREA-START'.         
007800     SKIP2                                                                
007900*01  AREA -COPY W2217701   -PRE W22177-                                   
008000                                                                          
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  KEYS-TILL-DLI.                                                       
008500     03  W-IDARTNR-X.                                                     
008600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008700     03  W-IDKORTNR-X.                                                    
008800         05  W-IDKORTNR          PIC X(23)    VALUE SPACE.                
008900     03  W-KDSEGKEY-X.                                                    
009008         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
009100     03  W-KDEMBAL-X.                                                     
009200         05  W-KDEMBAL           PIC X(3)    VALUE SPACE.                 
009304     03  W-WDG3KEY-X.                                                     
009404         05  FILLER              PIC X(4)    VALUE '2213'.                
009405         05  W-IDDC              PIC X(2)    VALUE '11'.                  
009504         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
009604*                                                                         
009700     SKIP2                                                                
009800*    --- STATUS-KOD FRÅN IMS                                              
009900 01  STATUS-WS                   PIC XX.                                  
010000     88  SEGMENT-FOUND                       VALUE '  '.                  
010100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010300     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
010400     88  IMS-NOT-OK                          VALUE 'XD'.                  
010500     SKIP2                                                                
010600 01  GOOD-STATUSCODES.                                                    
010700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010800     SKIP3                                                                
010900 01  SSA1                        PIC X(64).                               
011000 01  SSA2                        PIC X(64).                               
011100     EJECT                                                                
011200*    --- IMS FUNCTION CODES                                               
011300*01  -COPY W0003                                                          
011400     EJECT                                                                
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600                                                                          
011700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD701'.                      
011800 01  DLI-IO-WDD701.                                                       
011900*    03  -COPY WDD701                                                     
012000     EJECT                                                                
012100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD702'.                      
012200 01  DLI-IO-WDD702.                                                       
012300*    03  -COPY WDD702                                                     
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012500 01  DLI-IO-WDK601.                                                       
012600*    03  -COPY WDK601                                                     
012700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012800 01  DLI-IO-WDK611.                                                       
012900*    03  -COPY WDK611                                                     
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK613'.                      
013400 01  DLI-IO-WDK613.                                                       
013500*    03  -COPY WDK613                                                     
013704 01  DLI-IO-WDG302.                                                       
013804*    03  -COPY WDGX2214                                                   
013805 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301'.                      
013806 01  DLI-IO-WDT301.                                                       
013807*    03  -COPY WDT301                                                     
013808 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311'.                      
013809 01  DLI-IO-WDT311.                                                       
013810*    03  -COPY WDT311                                                     
014004*                                                                         
014100     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014300                                                                          
014400*01  -COPY W0009  -PRE MSG-                                               
014500                                                                          
014600*01  -COPY W0008  -PRE WDD7-                                              
014700     05  FILLER                  PIC X.                                   
014800                                                                          
014900*01  -COPY W0008  -PRE WDK6-                                              
015000     05  FILLER                  PIC X.                                   
015105                                                                          
015205*01  -COPY W0008  -PRE WDG3-                                              
015305     05  FILLER                  PIC X.                                   
015306                                                                          
015307*01  -COPY W0008  -PRE WDT3-                                              
015308     05  FILLER                  PIC X.                                   
015405     EJECT                                                                
015506 PROCEDURE DIVISION  USING MSG-PCB WDD7-PCB WDK6-PCB WDG3-PCB             
015507                                   WDT3-PCB.                              
015605 MAIN SECTION.                                                            
015706     ENTRY 'DLITCBL' USING MSG-PCB WDD7-PCB WDK6-PCB WDG3-PCB             
015707                                   WDT3-PCB.                              
015805                                                                          
015905     SKIP2                                                                
016005     PERFORM A-INIT                                                       
016105     PERFORM S01-READ-W22177                                              
016205     PERFORM UNTIL END-OF-W22177                                          
016305       IF CHKP-ANT > CHKP-MAX                                             
016405         PERFORM X-TAKE-CHECKPOINT                                        
016505       END-IF                                                             
016605       MOVE W22177-IDARTNR TO W-IDARTNR                                   
016705       PERFORM IMS-GU-WDD701                                              
016805       IF SEGMENT-FOUND                                                   
016806**LÄSER WDK6 BARA FÖR ATT FÅ KVULOAD FÖR DEN ERSATTA ARTIKELN             
016807         PERFORM IMS-GHU-WDK611                                           
016808         MOVE ZERO TO WS-KVULOAD                                          
016809         IF SEGMENT-FOUND                                                 
016810           MOVE CLAG-KVULOAD     TO WS-KVULOAD                            
016811         END-IF                                                           
016820**                                                                        
016918         PERFORM IMS-GNP-WDD702                                           
017005         PERFORM UNTIL SEGMENT-MISSING                                    
017205           IF SEGMENT-FOUND AND FLTEXT NOT = 'J'                          
017310             MOVE IDARTNR-TILLK TO W-IDARTNR                              
017409             PERFORM IMS-GHU-WDK601                                       
017507             IF SEGMENT-FOUND                                             
017609               PERFORM IMS-GHNP-WDK611                                    
017707               PERFORM S02-UPDATE-WDK613-Q0                               
017807               PERFORM S02-UPDATE-WDK613-Q1                               
017907               PERFORM S02-UPDATE-WDK613-Q2                               
018007               PERFORM S02-UPDATE-WDK613-Q3                               
018107               PERFORM S03-INSERT-WDT311-WDG302                           
018213               PERFORM IMS-GHU-WDK611                                     
018314               IF SEGMENT-FOUND                                           
018416                 PERFORM S05-COPY-FROM-W22177                             
018514                 PERFORM IMS-REPL-WDK611                                  
018614               END-IF                                                     
018714             END-IF                                                       
018814           END-IF                                                         
018918           PERFORM IMS-GNP-WDD702                                         
019014         END-PERFORM                                                      
019114       END-IF                                                             
019214       PERFORM S01-READ-W22177                                            
019314     END-PERFORM                                                          
019414                                                                          
019514     PERFORM Z-FINIT                                                      
019614                                                                          
019714     MOVE ZERO TO RETURN-CODE                                             
019814     GOBACK                                                               
019914     .                                                                    
020014     EJECT                                                                
020114 A-INIT SECTION.                                                          
020214     PERFORM IMS-RESTART                                                  
020314     OPEN INPUT W22177                                                    
020414     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020514     .                                                                    
020614     EJECT                                                                
020714 Z-FINIT SECTION.                                                         
020814     MOVE 'S' TO POSTSUM-OPKOD                                            
020914     CALL POSTSUM USING POSTSUM-PARM                                      
021014     .                                                                    
021114     EJECT                                                                
021214 S01-READ-W22177  SECTION.                                                
021314     READ W22177 INTO W22177-AREA                                         
021414     AT END                                                               
021514       SET END-OF-W22177 TO TRUE                                          
021614     NOT AT END                                                           
021714       MOVE 'W22178' TO POSTSUM-FDNAMN                                    
021814       MOVE 'W22178D1' TO POSTSUM-DDNAMN2                                 
021914       CALL POSTSUM USING POSTSUM-PARM                                    
022014     END-READ                                                             
022114     .                                                                    
022214     EJECT                                                                
022314 S02-UPDATE-WDK613-Q0 SECTION.                                            
022414     IF (W22177-IDARTNR-EMBQ0 > 0 AND CLAG-IDARTNR-EMBQ0 = 0) OR          
022514        (W22177-KDEMBKOD-0    > 0 AND CLAG-KDEMBKOD-0   = 0) OR           
022614        (W22177-KVQPACK-0     > 0 AND CLAG-KVQPACK-0    = 0)              
022717       MOVE 'Q0 ' TO W-KDEMBAL                                            
023614       PERFORM IMS-GHNP-WDK613                                            
023714       IF SEGMENT-FOUND                                                   
024216         IF (W22177-KDEMBKOD-0 > 0 AND CLAG-KDEMBKOD-0 = 0)               
024316           MOVE W22177-KDEMBKOD-0  TO EMB-KDEMBKOD                        
024416         END-IF                                                           
024516         IF (W22177-KVQPACK-0  > 0 AND CLAG-KVQPACK-0 = 0)                
024616           MOVE W22177-KVQPACK-0   TO EMB-KVQPACK-EMB                     
024716         END-IF                                                           
024814         PERFORM IMS-REPL-WDK613                                          
024914       ELSE                                                               
025014         MOVE 'Q0 '                TO EMB-KDEMBKEY                        
025114         MOVE ZERO                 TO EMB-IDARTNR-EMB                     
025214         MOVE W22177-KDEMBKOD-0    TO EMB-KDEMBKOD                        
025314         MOVE W22177-KVQPACK-0     TO EMB-KVQPACK-EMB                     
025414         PERFORM IMS-ISRT-WDK613                                          
025514       END-IF                                                             
025614     END-IF                                                               
025714     .                                                                    
025814     EJECT                                                                
025914 S02-UPDATE-WDK613-Q1 SECTION.                                            
026014     IF (W22177-IDARTNR-EMBQ1 > 0 AND CLAG-IDARTNR-EMBQ1 = 0) OR          
026114        (W22177-KDEMBKOD-1    > 0 AND CLAG-KDEMBKOD-1   = 0) OR           
026214        (W22177-KVQPACK-1     > 0 AND CLAG-KVQPACK-1    = 0)              
026317       MOVE 'Q1 ' TO W-KDEMBAL                                            
027314       PERFORM IMS-GHNP-WDK613                                            
027414       IF SEGMENT-FOUND                                                   
027916         IF (W22177-KDEMBKOD-1 > 0 AND CLAG-KDEMBKOD-1 = 0)               
028016           MOVE W22177-KDEMBKOD-1  TO EMB-KDEMBKOD                        
028116         END-IF                                                           
028216         IF (W22177-KVQPACK-1  > 0 AND CLAG-KVQPACK-1 = 0)                
028316           MOVE W22177-KVQPACK-1   TO EMB-KVQPACK-EMB                     
028416         END-IF                                                           
028514         PERFORM IMS-REPL-WDK613                                          
028614       ELSE                                                               
028714         MOVE 'Q1 '                TO EMB-KDEMBKEY                        
028715         MOVE ZERO                 TO EMB-IDARTNR-EMB                     
028914         MOVE W22177-KDEMBKOD-1    TO EMB-KDEMBKOD                        
029014         MOVE W22177-KVQPACK-1     TO EMB-KVQPACK-EMB                     
029114         PERFORM IMS-ISRT-WDK613                                          
029214       END-IF                                                             
029314     END-IF                                                               
029414     .                                                                    
029514     EJECT                                                                
029614 S02-UPDATE-WDK613-Q2 SECTION.                                            
029714     IF (W22177-IDARTNR-EMBQ2 > 0 AND CLAG-IDARTNR-EMBQ2 = 0) OR          
029814        (W22177-KVQPACK-2     > 0 AND CLAG-KVQPACK-2    = 0)              
029917       MOVE 'Q2 ' TO W-KDEMBAL                                            
030614       PERFORM IMS-GHNP-WDK613                                            
030714       IF SEGMENT-FOUND                                                   
031216         IF (W22177-KVQPACK-2  > 0 AND CLAG-KVQPACK-2 = 0)                
031316           MOVE W22177-KVQPACK-2   TO EMB-KVQPACK-EMB                     
031416         END-IF                                                           
031514         PERFORM IMS-REPL-WDK613                                          
031614       ELSE                                                               
031714         MOVE 'Q2 '                TO EMB-KDEMBKEY                        
031715         MOVE ZERO                 TO EMB-IDARTNR-EMB                     
031914         MOVE ZERO                 TO EMB-KDEMBKOD                        
032014         MOVE W22177-KVQPACK-2     TO EMB-KVQPACK-EMB                     
032114         PERFORM IMS-ISRT-WDK613                                          
032214       END-IF                                                             
032314     END-IF                                                               
032414     .                                                                    
032514     EJECT                                                                
032614 S02-UPDATE-WDK613-Q3 SECTION.                                            
032714     IF (W22177-IDARTNR-EMBQ3 > 0 AND CLAG-IDARTNR-EMBQ3 = 0) OR          
032814        (W22177-KVQPACK-3     > 0 AND CLAG-KVQPACK-3    = 0)              
032917       MOVE 'Q3 ' TO W-KDEMBAL                                            
033614       PERFORM IMS-GHNP-WDK613                                            
033714       IF SEGMENT-FOUND                                                   
033916         IF (W22177-IDARTNR-EMBQ3 > 0 AND CLAG-IDARTNR-EMBQ3 = 0)         
034016           MOVE W22177-IDARTNR-EMBQ3  TO EMB-IDARTNR-EMB                  
034116         END-IF                                                           
034216         IF (W22177-KVQPACK-3  > 0 AND CLAG-KVQPACK-3 = 0)                
034316           MOVE W22177-KVQPACK-3   TO EMB-KVQPACK-EMB                     
034416         END-IF                                                           
034514         PERFORM IMS-REPL-WDK613                                          
034614       ELSE                                                               
034714         MOVE 'Q3 '                TO EMB-KDEMBKEY                        
034814         MOVE W22177-IDARTNR-EMBQ3 TO EMB-IDARTNR-EMB                     
034914         MOVE ZERO                 TO EMB-KDEMBKOD                        
035014         MOVE W22177-KVQPACK-3     TO EMB-KVQPACK-EMB                     
035114         PERFORM IMS-ISRT-WDK613                                          
035214       END-IF                                                             
035314     END-IF                                                               
035414     .                                                                    
035514     EJECT                                                                
035614 S03-INSERT-WDT311-WDG302    SECTION.                                     
035616     PERFORM IMS-GU-WDT301                                                
035618     IF SEGMENT-MISSING                                                   
035620        MOVE W-IDARTNR TO FART-IDARTNR                                    
035621        PERFORM IMS-ISRT-WDT301                                           
035630     END-IF                                                               
035714     IF (W22177-BEFT = 91 OR 92 OR 96) AND CLAG-BEFT = 0                  
035814       MOVE 'W22178' TO FPCK-IDUSER                                       
035914       MOVE 90       TO FPCK-BEFT                                         
036014       MOVE ZERO     TO FPCK-KDFORPPL                                     
036114       MOVE ZERO     TO FPCK-KDFORPGP                                     
036214       MOVE ZERO     TO FPCK-KDFORPUF                                     
036314       MOVE SPACE    TO FPCK-TEBEFT(1)                                    
036414                        FPCK-TEBEFT(2)                                    
036514                        FPCK-TEBEFT(3)                                    
036515                        FPCK-TEBEFT(4)                                    
036516                        FPCK-TEBEFT(5)                                    
036614       MOVE FUNCTION CURRENT-DATE(1:8) TO W-DAREGDAT                      
036714       COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - W-DAREGDAT              
036814       ACCEPT W-TID FROM TIME                                             
036914       COMPUTE FPCK-TIKLOCK-9KOMPL  = 999999999 - W-TID                   
036915       MOVE 'SE'     TO FPCK-IDLANDX2                                     
037014       PERFORM IMS-ISRT-WDT311                                            
037115       MOVE W-IDARTNR TO 2214-IDARTNR                                     
037214       PERFORM IMS-ISRT-WDG302                                            
037314     END-IF                                                               
037315                                                                          
037530     IF W22177-FLMANLT = YES AND CLAG-FLMANLT = NOO                       
037617       MOVE W-IDARTNR TO 2214-IDARTNR                                     
037618       PERFORM IMS-ISRT-WDG302                                            
037619     END-IF                                                               
037620     .                                                                    
037621     EJECT                                                                
037630 S05-COPY-FROM-W22177 SECTION.                                            
038914     IF CLAG-IDARTNR-EMBQ3 = 0                                            
039014       MOVE W22177-IDARTNR-EMBQ3  TO CLAG-IDARTNR-EMBQ3                   
039114     END-IF                                                               
039214                                                                          
039314     IF CLAG-IDARTNR-EMBQ4 = 0                                            
039414       MOVE W22177-IDARTNR-EMBQ4  TO CLAG-IDARTNR-EMBQ4                   
039514     END-IF                                                               
039614                                                                          
039714     IF CLAG-KDEMBKOD-0 = 0                                               
039814       MOVE W22177-KDEMBKOD-0     TO CLAG-KDEMBKOD-0                      
039914     END-IF                                                               
040014                                                                          
040114     IF CLAG-KDEMBKOD-1 = 0                                               
040214       MOVE W22177-KDEMBKOD-1     TO CLAG-KDEMBKOD-1                      
040314     END-IF                                                               
040414                                                                          
040514     IF CLAG-KVQPACK-0 = 0                                                
040614       MOVE W22177-KVQPACK-0      TO CLAG-KVQPACK-0                       
040714     END-IF                                                               
040814                                                                          
040914     IF CLAG-KVQPACK-1 = 0                                                
041014       MOVE W22177-KVQPACK-1      TO CLAG-KVQPACK-1                       
041114     END-IF                                                               
041214                                                                          
041314     IF CLAG-KVQPACK-2 = 0                                                
041414       MOVE W22177-KVQPACK-2      TO CLAG-KVQPACK-2                       
041514     END-IF                                                               
041614                                                                          
041714     IF CLAG-KVQPACK-3 = 0                                                
041814       MOVE W22177-KVQPACK-3      TO CLAG-KVQPACK-3                       
041914     END-IF                                                               
042014                                                                          
042114     IF CLAG-KVQPACK-4 = 0                                                
042214       MOVE W22177-KVQPACK-4      TO CLAG-KVQPACK-4                       
042314     END-IF                                                               
042414                                                                          
042514     IF CLAG-KVPALL = 0                                                   
042614       MOVE W22177-KVPALL         TO CLAG-KVPALL                          
042714     END-IF                                                               
042814                                                                          
042914     IF W22177-FLMANLT = 'J' AND CLAG-FLMANLT = 'N'                       
042916       MOVE W22177-FLMANLT      TO CLAG-FLMANLT                           
043114       MOVE W22177-KVVECKOR-LT  TO CLAG-KVVECKOR-LT                       
043314     END-IF                                                               
043414                                                                          
043514     IF CLAG-BEFT = 0                                                     
043614        IF W22177-BEFT = 91 OR 92 OR 96                                   
043714           MOVE 90  TO CLAG-BEFT                                          
043814        END-IF                                                            
043914     END-IF                                                               
043915                                                                          
043916     IF CLAG-KVULOAD = 0                                                  
043920       MOVE WS-KVULOAD        TO CLAG-KVULOAD                             
043930     END-IF                                                               
044014     .                                                                    
044114     EJECT                                                                
044214 X-TAKE-CHECKPOINT   SECTION.                                             
044314* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
044414* --- SAVE DATABASE KEYS IF NECESSARY                                     
044514     PERFORM IMS-CHECKPOINT                                               
044614     MOVE ZERO TO CHKP-ANT                                                
044714* --- REREAD DATABASE IF NECESSARY                                        
044814     .                                                                    
044914     EJECT                                                                
045014* --- IMS SECTIONS  ---                                                   
045114                                                                          
045214     EJECT                                                                
045314 IMS-GU-WDD701 SECTION.                                                   
045414     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
045514          DELIMITED BY SIZE INTO SSA1                                     
045614     MOVE '  GE' TO GOOD-STATUSCODES                                      
045714     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
045814     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
045914     PERFORM IMS-STATUSCHECK                                              
046014     .                                                                    
046114     EJECT                                                                
046214 IMS-GNP-WDD702 SECTION.                                                  
046314     MOVE 'WDD702' TO SSA1                                                
046414     MOVE '  GE' TO GOOD-STATUSCODES                                      
046514     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
046614     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
046714     PERFORM IMS-STATUSCHECK                                              
046814     .                                                                    
046914     EJECT                                                                
047014 IMS-GHU-WDK601 SECTION.                                                  
047114     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
047214          DELIMITED BY SIZE INTO SSA1                                     
047314     MOVE '  GE' TO GOOD-STATUSCODES                                      
047414     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1                   
047514     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
047614     PERFORM IMS-STATUSCHECK                                              
047714     .                                                                    
047814     EJECT                                                                
047914 IMS-GHU-WDK611 SECTION.                                                  
048014     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
048114          DELIMITED BY SIZE INTO SSA1                                     
048214     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
048314          DELIMITED BY SIZE INTO SSA2                                     
048414     MOVE '  GE' TO GOOD-STATUSCODES                                      
048514     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
048614     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
048714     PERFORM IMS-STATUSCHECK                                              
048814     .                                                                    
048914     EJECT                                                                
049014 IMS-GHNP-WDK611 SECTION.                                                 
049114     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
049214          DELIMITED BY SIZE INTO SSA1                                     
049314     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
049414          DELIMITED BY SIZE INTO SSA2                                     
049514     MOVE '  GE' TO GOOD-STATUSCODES                                      
049614     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1 SSA2             
049714     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
049814     PERFORM IMS-STATUSCHECK                                              
049914     .                                                                    
050014     EJECT                                                                
050114 IMS-REPL-WDK611 SECTION.                                                 
050214     MOVE '  ' TO GOOD-STATUSCODES                                        
050314     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
050414     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
050514     PERFORM IMS-STATUSCHECK                                              
050618     ADD +2 TO CHKP-ANT                                                   
050714     .                                                                    
050814     EJECT                                                                
052014 IMS-GHNP-WDK613      SECTION.                                            
052114     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
052214          DELIMITED BY SIZE INTO SSA1                                     
052314     STRING 'WDK613  (KDEMBAL  =' W-KDEMBAL-X ')'                         
052414          DELIMITED BY SIZE INTO SSA2                                     
052514     MOVE '  GE' TO GOOD-STATUSCODES                                      
052614     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK613 SSA1 SSA2             
052714     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
052814     PERFORM IMS-STATUSCHECK                                              
052914     .                                                                    
053014     SKIP3                                                                
053114 IMS-REPL-WDK613 SECTION.                                                 
053214     MOVE '  ' TO GOOD-STATUSCODES                                        
053314     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK613                       
053414     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
053514     PERFORM IMS-STATUSCHECK                                              
053618     ADD +2 TO CHKP-ANT                                                   
053714     .                                                                    
053814     SKIP3                                                                
053914 IMS-ISRT-WDK613 SECTION.                                                 
054014     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
054114          DELIMITED BY SIZE INTO SSA1                                     
054214     MOVE 'WDK613   ' TO SSA2                                             
054314     MOVE '  II' TO GOOD-STATUSCODES                                      
054414     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK613 SSA1 SSA2             
054514     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
054614     PERFORM IMS-STATUSCHECK                                              
054718     ADD +2 TO CHKP-ANT                                                   
054814     .                                                                    
054914     EJECT                                                                
054915 IMS-GU-WDT301 SECTION.                                                   
054916     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
054917          DELIMITED BY SIZE INTO SSA1                                     
054918     MOVE '  GE' TO GOOD-STATUSCODES                                      
054919     CALL CBLTDLI USING GU WDT3-PCB DLI-IO-WDT301 SSA1                    
054920     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
054921     PERFORM IMS-STATUSCHECK                                              
054922     .                                                                    
054923     EJECT                                                                
054924 IMS-ISRT-WDT301 SECTION.                                                 
054927     MOVE 'WDT301   ' TO SSA1                                             
054928     MOVE '    ' TO GOOD-STATUSCODES                                      
054929     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
054930     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
054940     PERFORM IMS-STATUSCHECK                                              
054950     ADD +1 TO CHKP-ANT                                                   
054960     .                                                                    
054970     SKIP3                                                                
054980 IMS-ISRT-WDT311 SECTION.                                                 
054990     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
055000          DELIMITED BY SIZE INTO SSA1                                     
055010     MOVE 'WDT311   ' TO SSA2                                             
055011     MOVE '  II' TO GOOD-STATUSCODES                                      
055012     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
055013     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
055014     PERFORM IMS-STATUSCHECK                                              
055015     ADD +1 TO CHKP-ANT                                                   
055016     .                                                                    
055017     SKIP3                                                                
055018 IMS-ISRT-WDG302 SECTION.                                                 
055114     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY-X ')'                         
055214             DELIMITED BY SIZE INTO SSA1                                  
055314     MOVE 'WDG302' TO SSA2                                                
055414     MOVE '  IIGE' TO GOOD-STATUSCODES                                    
055514     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDG302 SSA1 SSA2             
055614     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
055714     PERFORM IMS-STATUSCHECK                                              
055818     ADD +1 TO CHKP-ANT                                                   
055914     .                                                                    
056014     EJECT                                                                
056114*                                                                         
056214 IMS-RESTART SECTION.                                                     
056314     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
056414     MOVE '  ' TO GOOD-STATUSCODES                                        
056514     CALL CBLTDLI USING XRST MSG-PCB                                      
056614                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
056714                        CHKP-AREA-LENGTH CHKP-AREA                        
056814     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056914     PERFORM IMS-STATUSCHECK                                              
057014     .                                                                    
057114     SKIP3                                                                
057214 IMS-CHECKPOINT SECTION.                                                  
057314     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
057414     MOVE '  XD' TO GOOD-STATUSCODES                                      
057514     CALL CBLTDLI USING CHKP MSG-PCB                                      
057614                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
057714                        CHKP-AREA-LENGTH CHKP-AREA                        
057814     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057914     PERFORM IMS-STATUSCHECK                                              
058014                                                                          
058114     IF IMS-NOT-OK                                                        
058214       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO                     
058314                                      ERROR-TEXT-STR                      
058414       DISPLAY ERROR-TEXT                                                 
058514       CALL FELLOG                                                        
058614     END-IF                                                               
058714     .                                                                    
058814     EJECT                                                                
058914 IMS-STATUSCHECK SECTION.                                                 
059014     SET STATUS-IX TO 1                                                   
059114     SEARCH GOOD-STATUS                                                   
059214       AT END                                                             
059314         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
059414           DELIMITED BY SIZE INTO ERROR-TEXT                              
059514         DISPLAY ERROR-TEXT                                               
059614         CALL FELLOG                                                      
059714       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
059814         CONTINUE                                                         
059912     END-SEARCH                                                           
060000     .                                                                    
