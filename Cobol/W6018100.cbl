000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6018100.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   99/07/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        VISAR OCH/ELLER UPPDATERAR FÖRPACKNINGSTYP                       
000900*                                                                         
001000*        THE PROGRAM UPDATES   WLARTC (WDK6)                              
001100*        THE PROGRAM READS     WLBENA (WDD3)                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W6T181                                              
001500*        MID:         W6I18101                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W6O18101                                            
001900*                                                                         
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W6018100'.            
002800                                                                          
002900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  YES                         PIC X       VALUE 'Y'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400                                                                          
003500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003600                                                                          
003700                                                                          
003800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
003900     88  OWN-MID                             VALUE '6181'.                
004000     88  GOOD-MID                            VALUE '6181' '6182'          
004100                                                   '6183' '6184'          
004200                                                   '6185'.                
004300     88  HELP-MID                            VALUE '0551'.                
004400                                                                          
004500*    --- WORKING STORAGE FIELDS                                           
004600                                                                          
004700 77  INDX                        PIC S9(4) VALUE +0 COMP SYNC.            
004800 77  MAX-INDX                    PIC S9(4) VALUE +2 COMP SYNC.            
004900 77  MAX-KVRADER                 PIC S9(4) VALUE +2 COMP.                 
005000 01  ALL-PLUS.                                                            
005100     03 FILLER                   PIC X(80) VALUE ALL '+'.                 
005200     EJECT                                                                
005300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  W6018110                PIC X(8)    VALUE 'W6018110'.            
005900     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
006000     EJECT                                                                
006100 01  MESSAGE-CODES.                                                       
006200     03  FOR-MORE-INFORMATION    PIC X(3)    VALUE '105'.                 
006300*                                                                         
006400 01  W-IDMSG-ERROR               PIC X(3).                                
006500     88   WRONG-KEY                          VALUE '022'.                 
006600     88   PART-MISSING                       VALUE '025'.                 
006700                                                                          
006800*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
006900*                                                                         
007000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007100     SKIP3                                                                
007200*01 -COPY WMSGINIT                                                        
007300     EJECT                                                                
007400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
007500*                                                                         
007600 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
007700 01  REQU-AREA.                                                           
007800*    03 -COPY WZ01REQU                                                    
007900*    03 -COPY W60181I1                                                    
008000     EJECT                                                                
008100*                                                                         
008200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
008300 01  RESP-AREA.                                                           
008400*    03 -COPY WZ01RESP                                                    
008500*    03 -COPY W60181O1                                                    
008600*                                                                         
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008900     SKIP3                                                                
009000*01  MID -COPY W6I18101                                                   
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009300     SKIP3                                                                
009400*01  -COPY WMSGAREA                                                       
009500     EJECT                                                                
009600     03  MOD REDEFINES MSG-AREA.                                          
009700*      05  -COPY W6O18101                                                 
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010000     SKIP3                                                                
010100*01  -COPY WMFSAREA                                                       
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
010400     SKIP3                                                                
010500*01  -COPY WL01MCNV                                                       
010600     EJECT                                                                
010700     EJECT                                                                
010800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010900*                                                                         
011000     EJECT                                                                
011100     SKIP2                                                                
011200 01  SPAR-AREA.                                                           
011300     03  SPAR-IDTRANS               PIC X(4)    VALUE '6181'.             
011400     03  SPAR-IDDC                  PIC X(2).                             
011500     03  SPAR-DAREGDAT-NEXT         PIC 9(8).                             
011600     03  SPAR-TIKLOCK-NEXT          PIC S9(9) COMP-3.                     
011700     EJECT                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FOUND                       VALUE '  '.                  
012100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GOOD-STATUSCODES.                                                    
012500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700     EJECT                                                                
012800*    --- IMS FUNCTION CODES                                               
012900*01  -COPY W0003                                                          
013000     EJECT                                                                
013100 LINKAGE SECTION.                                                         
013200*01  -COPY W0009  -PRE MSG-                                               
013300     EJECT                                                                
013400*01  -COPY W0009  -PRE DISP-                                              
013500     EJECT                                                                
013600*01  -COPY W0008  -PRE USEA-                                              
013700     05  FILLER                  PIC X.                                   
013800                                                                          
013900*01  -COPY W0008  -PRE ARTC-                                              
014000     05  FILLER                  PIC X.                                   
014100                                                                          
014200*01  -COPY W0008  -PRE BENA-                                              
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500*01  -COPY W0008  -PRE XXBI-                                              
014600     05 FILLER                   PIC X.                                   
014700     EJECT                                                                
014800*01  -COPY W0008  -PRE WDT3-                                              
014900     05 FILLER                   PIC X.                                   
015000*01  -COPY W0008  -PRE WDK7-                                              
015100     05 FILLER                   PIC X.                                   
015200 01 WDB6-PCB                     PIC X.                                   
015300                                                                          
015400*    PCB'ER FÖR SUBPGM                                                    
015500                                                                          
015600 01 KOM-KOMA-PCB                 PIC X.                                   
015700     EJECT                                                                
015800 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB USEA-PCB ARTC-PCB             
015900                           BENA-PCB XXBI-PCB WDT3-PCB WDK7-PCB            
016000                           WDB6-PCB                                       
016100                           KOM-KOMA-PCB.                                  
016200 MAIN SECTION.                                                            
016300     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB USEA-PCB ARTC-PCB             
016400                           BENA-PCB XXBI-PCB WDT3-PCB WDK7-PCB            
016500                           WDB6-PCB                                       
016600                           KOM-KOMA-PCB.                                  
016700     PERFORM IMS-GET-MSG                                                  
016800     IF SEGMENT-FOUND                                                     
016900                                                                          
017000       PERFORM A-INIT                                                     
017100       PERFORM B-INIT-KEYS                                                
017200                                                                          
017300       IF MFS-UPDATE                                                      
017400         SET REQU-UPDATE       TO TRUE                                    
017500       ELSE                                                               
017600         IF MFS-UPD-V                                                     
017700             SET REQU-UPD-V    TO TRUE                                    
017800         ELSE                                                             
017900           IF MFS-FIRST                                                   
018000              SET REQU-FIRST   TO TRUE                                    
018100              PERFORM MFS-ERASE-FIELD-IN                                  
018200           ELSE                                                           
018300             IF MFS-ENTER                                                 
018400                SET REQU-QUERY TO TRUE                                    
018500                PERFORM E-SAME-PAGE                                       
018600             END-IF                                                       
018700             IF MFS-NEXT                                                  
018800                SET REQU-NEXT  TO TRUE                                    
018900                PERFORM D-NEXT-PAGE                                       
019000             END-IF                                                       
019100           END-IF                                                         
019200         END-IF                                                           
019300       END-IF                                                             
019400                                                                          
019500       PERFORM F-CALL-BIZ-LOGIC-W6018110                                  
019600                                                                          
019700       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O18101 + 4                      
019800       PERFORM IMS-INSERT-MSG                                             
019900     END-IF                                                               
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK                                                               
020300     .                                                                    
020400     EJECT                                                                
020500                                                                          
020600 A-INIT SECTION.                                                          
020700                                                                          
020800     IF MSG-DOUBLE-TRANSACTIONS                                           
020900       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I18101                 
021000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
021100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
021200     ELSE                                                                 
021300       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W6I18101                 
021400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
021500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
021600     END-IF                                                               
021700                                                                          
021800     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
021900     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
022000     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
022100                                                                          
022200     MOVE LOW-VALUE                       TO MSG-AREA                     
022300     MOVE 'W6O181N1'                      TO MFS-IDMOD                    
022400     MOVE '6181'                          TO MOD-IDTRANS                  
022500     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
022600                                             MOD-TEMFSINF                 
022700     IF MSGI-IDLAND-SPR = 'SE'                                            
022800        MOVE '0'                          TO MFS-KDHUVOMR                 
022900     END-IF                                                               
023000                                                                          
023100                                                                          
023200     IF OWN-MID OR HELP-MID                                               
023300       CONTINUE                                                           
023400     ELSE                                                                 
023500       MOVE SPACE                         TO MFS-KDTRTYP                  
023600       MOVE '7'                           TO MFS-IDPFK                    
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024000                                                                          
024100 B-INIT-KEYS SECTION.                                                     
024200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024300     MOVE '001'             TO MSGI-KDCALL                                
024400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024600                               REQU-IDUSER-IN                             
024700     MOVE '6181'            TO MSGI-IDTRANS                               
024800                                                                          
024900     IF GOOD-MID                                                          
025000        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
025100     END-IF                                                               
025200                                                                          
025300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025400     IF OWN-MID                                                           
025500        MOVE MSGI-SPAR-AREA TO SPAR-AREA                                  
025600     END-IF                                                               
025700*                                                                         
025800     MOVE MFS-ERASE-FIELD   TO MOD-IDARTNR-IN                             
025900     IF MID-IDARTNR-IN NOT = ALL '+'                                      
026000       MOVE '7'             TO MFS-IDPFK                                  
026100       MOVE SPACE           TO MFS-KDTRTYP                                
026200     END-IF                                                               
026300*                                                                         
026400     MOVE MSGI-IDARTNR      TO MOD-IDARTNR-UT                             
026500     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
026600                                                                          
026700     MOVE MSGI-IDARTNR      TO REQU-IDARTNR-KEY                           
026800     MOVE MSGI-IDDC         TO REQU-IDDC                                  
026900     IF MID-IDDC-KEY-IN NUMERIC                                           
027000        MOVE MID-IDDC-KEY-IN TO REQU-IDDC-KEY                             
027100     ELSE                                                                 
027200       IF MID-IDDC-KEY-UT NUMERIC                                         
027210          MOVE MID-IDDC-KEY-UT TO REQU-IDDC-KEY                           
027220       ELSE                                                               
027300          MOVE MSGI-IDDC       TO REQU-IDDC-KEY                           
027310       END-IF                                                             
027400     END-IF                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 D-NEXT-PAGE SECTION.                                                     
027800                                                                          
027900     PERFORM MFS-ERASE-FIELD-IN                                           
028000                                                                          
028100     MOVE SPAR-DAREGDAT-NEXT   TO REQU-DAREGDAT-START                     
028200     IF SPAR-TIKLOCK-NEXT NUMERIC                                         
028300     MOVE SPAR-TIKLOCK-NEXT    TO REQU-TIKLOCK-START                      
028400     ELSE                                                                 
028500     MOVE ZEROS TO REQU-TIKLOCK-START                                     
028600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 E-SAME-PAGE SECTION.                                                     
029000                                                                          
029100     IF OWN-MID OR HELP-MID                                               
029200       CONTINUE                                                           
029300     ELSE                                                                 
029400       PERFORM MFS-ERASE-FIELD-IN                                         
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800                                                                          
029900 F-CALL-BIZ-LOGIC-W6018110 SECTION.                                       
030000                                                                          
030100     PERFORM FA-INIT-REQU                                                 
030200     CALL W6018110 USING REQU-AREA RESP-AREA MAX-KVRADER                  
030300                         MSG-PCB DISP-PCB  ARTC-PCB  BENA-PCB             
030400                         XXBI-PCB WDT3-PCB WDK7-PCB WDB6-PCB              
030500                         KOM-KOMA-PCB                                     
030600                                                                          
030700     PERFORM FB-SET-MSG-AND-HILIGHT                                       
030800     IF PART-MISSING AND RESP-KVRADER = 0                                 
030900        CONTINUE                                                          
031000     ELSE                                                                 
031100        IF NOT WRONG-KEY                                                  
031200           PERFORM FC-MOVE-RESP-TO-MOD                                    
031300        END-IF                                                            
031400     END-IF                                                               
031500     .                                                                    
031600     EJECT                                                                
031700 FA-INIT-REQU SECTION.                                                    
031800                                                                          
031900     MOVE MAX-KVRADER               TO REQU-KVRADER                       
032000                                                                          
032100     IF MID-BEFT = ALL '+'                                                
032200        MOVE ALL-PLUS               TO REQU-BEFT                          
032300     ELSE                                                                 
032400        MOVE MID-BEFT               TO REQU-BEFT                          
032500     END-IF                                                               
032600                                                                          
032700     IF MID-KDFORP = ALL '+'                                              
032800        MOVE ALL-PLUS               TO REQU-KDFORP                        
032900     ELSE                                                                 
033000        MOVE MID-KDFORP             TO REQU-KDFORP                        
033100     END-IF                                                               
033200                                                                          
033300     IF MID-TEBEFT = ALL '+'                                              
033400        MOVE ALL-PLUS               TO REQU-TEBEFT                        
033500     ELSE                                                                 
033600        MOVE MID-TEBEFT             TO REQU-TEBEFT                        
033700     END-IF                                                               
033800                                                                          
033900     IF MID-TEBEFT-79 = ALL '+'                                           
034000        MOVE ALL-PLUS               TO REQU-TEBEFT-79                     
034100     ELSE                                                                 
034200        MOVE MID-TEBEFT-79          TO REQU-TEBEFT-79                     
034300     END-IF                                                               
034400                                                                          
034500     IF MID-TEBEFT02-79 = ALL '+'                                         
034600        MOVE ALL-PLUS               TO REQU-TEBEFT02-79                   
034700     ELSE                                                                 
034800        MOVE MID-TEBEFT02-79        TO REQU-TEBEFT02-79                   
034900     END-IF                                                               
035000                                                                          
035100     MOVE '101'                     TO REQU-IDMSGVER                      
035200     MOVE MSGI-IDUSER               TO REQU-IDUSER                        
035300     MOVE MSGI-IDSPRAK              TO REQU-IDSPRAK                       
035400     .                                                                    
035500 FB-SET-MSG-AND-HILIGHT SECTION.                                          
035600                                                                          
035700     MOVE RESP-IDMSG-ERROR          TO MCNV-IDMSG-ERROR                   
035800                                       W-IDMSG-ERROR                      
035900     MOVE RESP-IDMSG-INFO           TO MCNV-IDMSG-INFO                    
036000     MOVE RESP-IDELMT-ERROR         TO MCNV-IDELMT-ERROR                  
036100     MOVE MSGI-IDSPRAK              TO MCNV-IDSPRAK                       
036200     IF WRONG-KEY                                                         
036300        PERFORM MFS-ERASE-FIELD-IN                                        
036400        PERFORM MFS-ERASE-FIELD-OUT                                       
036500     END-IF                                                               
036600                                                                          
036700     IF PART-MISSING                                                      
036800        IF RESP-KVRADER = 0                                               
036900           PERFORM MFS-ERASE-FIELD-OUT                                    
037000        END-IF                                                            
037100     END-IF                                                               
037200                                                                          
037300     CALL WL01MCNV USING MCNV-AREA                                        
037400                                                                          
037500     MOVE MCNV-MFSINF               TO MOD-TEMFSINF                       
037600     MOVE MCNV-MFSFEL               TO MOD-TEMFSFEL                       
037700     .                                                                    
037800     EJECT                                                                
037900 FC-MOVE-RESP-TO-MOD SECTION.                                             
038000                                                                          
038100     IF MOD-TEMFSINF(1:3) = FOR-MORE-INFORMATION                          
038200                                                                          
038300       MOVE RESP-DAREGDAT-NEXT            TO SPAR-DAREGDAT-NEXT           
038400       MOVE RESP-TIKLOCK-NEXT             TO SPAR-TIKLOCK-NEXT            
038500       MOVE '002'                         TO MSGI-KDCALL                  
038600       MOVE '6181'                        TO SPAR-IDTRANS                 
038700       MOVE SPAR-AREA                     TO MSGI-SPAR-AREA               
038800       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
038900     ELSE                                                                 
039000       MOVE ZEROS                         TO SPAR-DAREGDAT-NEXT           
039100                                             SPAR-TIKLOCK-NEXT            
039200       MOVE '002'                         TO MSGI-KDCALL                  
039300       MOVE '6181'                        TO SPAR-IDTRANS                 
039400       MOVE SPAR-AREA                     TO MSGI-SPAR-AREA               
039500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
039600     END-IF                                                               
039700                                                                          
039800     IF RESP-IDDC-KEY = SPACE                                             
039900        MOVE MFS-ERASE-FIELD            TO MOD-IDDC-UT                    
040000     ELSE                                                                 
040100        MOVE RESP-IDDC-KEY              TO MOD-IDDC-UT                    
040200     END-IF                                                               
040300                                                                          
040400     IF RESP-BEART-UT = SPACE                                             
040500        MOVE MFS-ERASE-FIELD              TO MOD-BEART-UT                 
040600     ELSE                                                                 
040700       IF RESP-BEART-UT = ALL '+'                                         
040800          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-BEART-UT                 
040900       ELSE                                                               
041000          MOVE RESP-BEART-UT              TO MOD-BEART-UT                 
041100       END-IF                                                             
041200     END-IF                                                               
041300                                                                          
041400*    IF RESP-DAREGDAT-NEXT NOT = ALL '+'                                  
041500       IF RESP-BEFT-UT = SPACE                                            
041600          MOVE MFS-ERASE-FIELD              TO MOD-BEFT-UT                
041700       ELSE                                                               
041800         IF RESP-BEFT-UT = ALL '+'                                        
041900            MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-BEFT-UT                
042000         ELSE                                                             
042100            MOVE RESP-BEFT-UT               TO MOD-BEFT-UT                
042200         END-IF                                                           
042300       END-IF                                                             
042400                                                                          
042500       IF RESP-KDFORP-UT = SPACE                                          
042600          MOVE MFS-ERASE-FIELD              TO MOD-KDFORP-UT              
042700       ELSE                                                               
042800         IF RESP-KDFORP-UT = ALL '+'                                      
042900            MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDFORP-UT              
043000         ELSE                                                             
043100            MOVE RESP-KDFORP-UT             TO MOD-KDFORP-UT              
043200         END-IF                                                           
043300       END-IF                                                             
043400                                                                          
043500       IF RESP-IDUSER-UT = SPACE                                          
043600          MOVE MFS-ERASE-FIELD              TO MOD-IDUSER-UT              
043700       ELSE                                                               
043800         IF RESP-IDUSER-UT = ALL '+'                                      
043900            MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDUSER-UT              
044000         ELSE                                                             
044100            MOVE RESP-IDUSER-UT             TO MOD-IDUSER-UT              
044200         END-IF                                                           
044300       END-IF                                                             
044400                                                                          
044500       IF RESP-TEBEFT-UT = SPACE                                          
044600          MOVE MFS-ERASE-FIELD              TO MOD-TEBEFT-UT              
044700       ELSE                                                               
044800         IF RESP-TEBEFT-UT =  ALL '+'                                     
044900            MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-TEBEFT-UT              
045000         ELSE                                                             
045100            MOVE RESP-TEBEFT-UT             TO MOD-TEBEFT-UT              
045200         END-IF                                                           
045300       END-IF                                                             
045400                                                                          
045500       IF RESP-TEBEFT-79-UT = SPACE                                       
045600          MOVE MFS-ERASE-FIELD              TO MOD-TEBEFT-79-UT           
045700       ELSE                                                               
045800         IF RESP-TEBEFT-79-UT = ALL '+'                                   
045900            MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-TEBEFT-79-UT           
046000         ELSE                                                             
046100            MOVE RESP-TEBEFT-79-UT          TO MOD-TEBEFT-79-UT           
046200         END-IF                                                           
046300       END-IF                                                             
046400                                                                          
046500       IF RESP-TEBEFT02-79-UT = SPACE                                     
046600          MOVE MFS-ERASE-FIELD              TO MOD-TEBEFT02-79-UT         
046700       ELSE                                                               
046800         IF RESP-TEBEFT02-79-UT = ALL '+'                                 
046900            MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-TEBEFT02-79-UT         
047000         ELSE                                                             
047100            MOVE RESP-TEBEFT02-79-UT        TO MOD-TEBEFT02-79-UT         
047200         END-IF                                                           
047300       END-IF                                                             
047400                                                                          
047500       IF RESP-TIREGDAT-UT = SPACE                                        
047600          MOVE MFS-ERASE-FIELD              TO MOD-TIREGDAT-UT            
047700       ELSE                                                               
047800         IF RESP-TIREGDAT-UT = ALL '+'                                    
047900            MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-TIREGDAT-UT            
048000         ELSE                                                             
048100            MOVE RESP-TIREGDAT-UT           TO MOD-TIREGDAT-UT            
048200         END-IF                                                           
048300       END-IF                                                             
048400*    END-IF                                                               
048500                                                                          
048600     MOVE RESP-BEFT-ATTR             TO MOD-BEFT-IN-ATTR                  
048700     MOVE RESP-KDFORP-ATTR           TO MOD-KDFORP-IN-ATTR                
048800     MOVE RESP-TEBEFT-ATTR           TO MOD-TEBEFT-IN-ATTR                
048900     MOVE RESP-TEBEFT-79-ATTR        TO MOD-TEBEFT-79-IN-ATTR             
049000     MOVE RESP-TEBEFT02-79-ATTR      TO MOD-TEBEFT02-79-IN-ATTR           
049100                                                                          
049200     IF RESP-BEFT = SPACE                                                 
049300        MOVE MFS-ERASE-FIELD            TO MOD-BEFT-IN                    
049400     ELSE                                                                 
049500       IF RESP-BEFT = ALL '+'                                             
049600          MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-BEFT-IN                    
049700       ELSE                                                               
049800          MOVE MFS-ERASE-FIELD          TO MOD-BEFT-IN                    
049900       END-IF                                                             
050000     END-IF                                                               
050100                                                                          
050200     IF RESP-KDFORP = SPACE                                               
050300        MOVE MFS-ERASE-FIELD            TO MOD-KDFORP-IN                  
050400     ELSE                                                                 
050500       IF RESP-KDFORP = ALL '+'                                           
050600          MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-KDFORP-IN                  
050700       ELSE                                                               
050800          MOVE MFS-ERASE-FIELD          TO MOD-KDFORP-IN                  
050900       END-IF                                                             
051000     END-IF                                                               
051100                                                                          
051200     IF RESP-TEBEFT = SPACE                                               
051300        MOVE MFS-ERASE-FIELD            TO MOD-TEBEFT-IN                  
051400     ELSE                                                                 
051500       IF RESP-TEBEFT = ALL '+'                                           
051600          MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-TEBEFT-IN                  
051700       ELSE                                                               
051800          MOVE MFS-ERASE-FIELD          TO MOD-TEBEFT-IN                  
051900       END-IF                                                             
052000     END-IF                                                               
052100                                                                          
052200     IF RESP-TEBEFT-79 = SPACE                                            
052300        MOVE MFS-ERASE-FIELD            TO MOD-TEBEFT-79-IN               
052400     ELSE                                                                 
052500       IF RESP-TEBEFT-79 = ALL '+'                                        
052600          MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-TEBEFT-79-IN               
052700       ELSE                                                               
052800          MOVE MFS-ERASE-FIELD          TO MOD-TEBEFT-79-IN               
052900       END-IF                                                             
053000     END-IF                                                               
053100                                                                          
053200     IF RESP-TEBEFT02-79 = SPACE                                          
053300        MOVE MFS-ERASE-FIELD            TO MOD-TEBEFT02-79-IN             
053400     ELSE                                                                 
053500       IF RESP-TEBEFT02-79 = ALL '+'                                      
053600          MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-TEBEFT02-79-IN             
053700       ELSE                                                               
053800          MOVE MFS-ERASE-FIELD          TO MOD-TEBEFT02-79-IN             
053900       END-IF                                                             
054000     END-IF                                                               
054100                                                                          
054200     MOVE +1  TO INDX                                                     
054300     PERFORM UNTIL INDX > RESP-KVRADER                                    
054400                                                                          
054500       IF RESP-BEFT-HIST-LINE(INDX) = SPACE                               
054600          MOVE MFS-ERASE-FIELD                TO                          
054700                                        MOD-BEFT-HIST(INDX)               
054800       ELSE                                                               
054900          IF RESP-BEFT-HIST-LINE(INDX) = ALL '+'                          
055000             MOVE MFS-DO-NOT-TOUCH-FIELD      TO                          
055100                                        MOD-BEFT-HIST(INDX)               
055200          ELSE                                                            
055300             MOVE RESP-BEFT-HIST-LINE(INDX)   TO                          
055400                                        MOD-BEFT-HIST(INDX)               
055500          END-IF                                                          
055600       END-IF                                                             
055700                                                                          
055800       IF RESP-KDFORP-HIST-LINE(INDX) = SPACE                             
055900          MOVE MFS-ERASE-FIELD            TO                              
056000                                     MOD-KDFORP-HIST(INDX)                
056100       ELSE                                                               
056200          IF RESP-KDFORP-HIST-LINE(INDX) = ALL '+'                        
056300             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
056400                                     MOD-KDFORP-HIST(INDX)                
056500          ELSE                                                            
056600             MOVE RESP-KDFORP-HIST-LINE(INDX) TO                          
056700                                     MOD-KDFORP-HIST(INDX)                
056800          END-IF                                                          
056900       END-IF                                                             
057000                                                                          
057100       IF RESP-IDUSER-HIST-LINE(INDX) = SPACE                             
057200          MOVE MFS-ERASE-FIELD            TO                              
057300                                     MOD-IDUSER-HIST(INDX)                
057400       ELSE                                                               
057500          IF RESP-IDUSER-HIST-LINE(INDX) = ALL '+'                        
057600             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
057700                                     MOD-IDUSER-HIST(INDX)                
057800          ELSE                                                            
057900             MOVE RESP-IDUSER-HIST-LINE(INDX) TO                          
058000                                     MOD-IDUSER-HIST(INDX)                
058100          END-IF                                                          
058200       END-IF                                                             
058300                                                                          
058400       IF RESP-TEBEFT-HIST-LINE(INDX) = SPACE                             
058500          MOVE MFS-ERASE-FIELD            TO                              
058600                                     MOD-TEBEFT-HIST(INDX)                
058700       ELSE                                                               
058800          IF RESP-TEBEFT-HIST-LINE(INDX) = ALL '+'                        
058900             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
059000                                     MOD-TEBEFT-HIST(INDX)                
059100          ELSE                                                            
059200             MOVE RESP-TEBEFT-HIST-LINE(INDX) TO                          
059300                                     MOD-TEBEFT-HIST(INDX)                
059400          END-IF                                                          
059500       END-IF                                                             
059600                                                                          
059700       IF RESP-TEBEFT-79-HIST-LINE(INDX) = SPACE                          
059800          MOVE MFS-ERASE-FIELD            TO                              
059900                                     MOD-TEBEFT-79-HIST(INDX)             
060000       ELSE                                                               
060100          IF RESP-TEBEFT-79-HIST-LINE(INDX) = ALL '+'                     
060200             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
060300                                     MOD-TEBEFT-79-HIST(INDX)             
060400          ELSE                                                            
060500             MOVE RESP-TEBEFT-79-HIST-LINE(INDX)                          
060600                                          TO                              
060700                                     MOD-TEBEFT-79-HIST(INDX)             
060800          END-IF                                                          
060900       END-IF                                                             
061000                                                                          
061100       IF RESP-TEBEFT02-79-HIST-LINE(INDX) = SPACE                        
061200          MOVE MFS-ERASE-FIELD            TO                              
061300                                     MOD-TEBEFT02-79-HIST(INDX)           
061400       ELSE                                                               
061500          IF RESP-TEBEFT02-79-HIST-LINE(INDX) = ALL '+'                   
061600             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
061700                                     MOD-TEBEFT02-79-HIST(INDX)           
061800          ELSE                                                            
061900             MOVE RESP-TEBEFT02-79-HIST-LINE(INDX)                        
062000                                          TO                              
062100                                     MOD-TEBEFT02-79-HIST(INDX)           
062200          END-IF                                                          
062300       END-IF                                                             
062400                                                                          
062500       IF RESP-TIREGDAT-HIST-LINE(INDX) = SPACE                           
062600          MOVE MFS-ERASE-FIELD            TO                              
062700                                     MOD-TIREGDAT-HIST(INDX)              
062800       ELSE                                                               
062900          IF RESP-TIREGDAT-HIST-LINE(INDX) = ALL '+'                      
063000             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
063100                                     MOD-TIREGDAT-HIST(INDX)              
063200          ELSE                                                            
063300             MOVE RESP-TIREGDAT-HIST-LINE(INDX)                           
063400                                          TO                              
063500                                     MOD-TIREGDAT-HIST(INDX)              
063600          END-IF                                                          
063700       END-IF                                                             
063800                                                                          
063900       ADD +1 TO INDX                                                     
064000     END-PERFORM                                                          
064100                                                                          
064200     PERFORM UNTIL INDX > MAX-KVRADER                                     
064300       MOVE MFS-ERASE-FIELD TO MOD-BEFT-HIST(INDX)                        
064400                               MOD-KDFORP-HIST(INDX)                      
064500                               MOD-IDUSER-HIST(INDX)                      
064600                               MOD-TEBEFT-HIST(INDX)                      
064700                               MOD-TEBEFT-79-HIST(INDX)                   
064800                               MOD-TEBEFT02-79-HIST(INDX)                 
064900                               MOD-TIREGDAT-HIST(INDX)                    
065000                                                                          
065100       ADD +1 TO INDX                                                     
065200     END-PERFORM                                                          
065300     .                                                                    
065400     EJECT                                                                
065500 MFS-ERASE-FIELD-OUT SECTION.                                             
065600*    --- ALLA UTDATA-FÄLT                                                 
065700     MOVE MFS-ERASE-FIELD TO MOD-BEART-UT                                 
065800                             MOD-BEFT-UT                                  
065900                             MOD-KDFORP-UT                                
066000                             MOD-IDUSER-UT                                
066100                             MOD-TIREGDAT-UT                              
066200                             MOD-TEBEFT-UT                                
066300                             MOD-TEBEFT-79-UT                             
066400                             MOD-TEBEFT02-79-UT                           
066500                             MOD-BEFT-HIST(1)                             
066600                             MOD-KDFORP-HIST(1)                           
066700                             MOD-IDUSER-HIST(1)                           
066800                             MOD-TIREGDAT-HIST(1)                         
066900                             MOD-TEBEFT-HIST(1)                           
067000                             MOD-TEBEFT-79-HIST(1)                        
067100                             MOD-TEBEFT02-79-HIST(1)                      
067200                             MOD-BEFT-HIST(2)                             
067300                             MOD-KDFORP-HIST(2)                           
067400                             MOD-IDUSER-HIST(2)                           
067500                             MOD-TIREGDAT-HIST(2)                         
067600                             MOD-TEBEFT-HIST(2)                           
067700                             MOD-TEBEFT-79-HIST(2)                        
067800                             MOD-TEBEFT02-79-HIST(2)                      
067900     .                                                                    
068000     SKIP3                                                                
068100 MFS-ERASE-FIELD-IN SECTION.                                              
068200*    --- ALLA INDATA-FÄLT                                                 
068300     MOVE MFS-ERASE-FIELD TO MOD-BEFT-IN                                  
068400                             MOD-KDFORP-IN                                
068500                             MOD-TEBEFT-IN                                
068600                             MOD-TEBEFT-79-IN                             
068700                             MOD-TEBEFT02-79-IN                           
068800     .                                                                    
068900     EJECT                                                                
069000 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
069100*    --- ALLA UTDATA-FÄLT                                                 
069200     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEART-UT                          
069300                                    MOD-BEFT-UT                           
069400                                    MOD-KDFORP-UT                         
069500                                    MOD-IDUSER-UT                         
069600                                    MOD-TIREGDAT-UT                       
069700                                    MOD-TEBEFT-UT                         
069800                                    MOD-TEBEFT-79-UT                      
069900                                    MOD-TEBEFT02-79-UT                    
070000                                    MOD-BEFT-HIST(1)                      
070100                                    MOD-KDFORP-HIST(1)                    
070200                                    MOD-IDUSER-HIST(1)                    
070300                                    MOD-TIREGDAT-HIST(1)                  
070400                                    MOD-TEBEFT-HIST(1)                    
070500                                    MOD-TEBEFT-79-HIST(1)                 
070600                                    MOD-TEBEFT02-79-HIST(1)               
070700                                    MOD-BEFT-HIST(2)                      
070800                                    MOD-KDFORP-HIST(2)                    
070900                                    MOD-IDUSER-HIST(2)                    
071000                                    MOD-TIREGDAT-HIST(2)                  
071100                                    MOD-TEBEFT-HIST(2)                    
071200                                    MOD-TEBEFT-79-HIST(2)                 
071300                                    MOD-TEBEFT02-79-HIST(2)               
071400     .                                                                    
071500     SKIP3                                                                
071600 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
071700*    --- ALLA INDATA-FÄLT                                                 
071800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEFT-IN                           
071900                                    MOD-KDFORP-IN                         
072000                                    MOD-TEBEFT-IN                         
072100                                    MOD-TEBEFT-79-IN                      
072200                                    MOD-TEBEFT02-79-IN                    
072300                                                                          
072400     .                                                                    
072500     EJECT                                                                
072600 MFS-FORM-ATTR SECTION.                                                   
072700*    --- ALL INDATA-FIELDS                                                
072800     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-BEFT-IN-ATTR                     
072900                                     MOD-KDFORP-IN-ATTR                   
073000                                     MOD-TEBEFT-IN-ATTR                   
073100                                     MOD-TEBEFT-79-IN-ATTR                
073200                                     MOD-TEBEFT02-79-IN-ATTR              
073300                                                                          
073400     .                                                                    
073500     SKIP2                                                                
073600* --- IMS SECTIONS ---                                                    
073700     SKIP3                                                                
073800 IMS-GET-MSG SECTION.                                                     
073900                                                                          
074000     MOVE '  QC'          TO GOOD-STATUSCODES                             
074100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
074200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074300     PERFORM IMS-STATUSCHECK                                              
074400     .                                                                    
074500     SKIP3                                                                
074600 IMS-INSERT-MSG SECTION.                                                  
074700                                                                          
074800     IF MSGI-IDLAND-SPR = 'SE'                                            
074900       MOVE '0'           TO MFS-KDHUVOMR                                 
075000     END-IF                                                               
075100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
075200     MOVE SPACE TO GOOD-STATUSCODES                                       
075300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
075400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075500     PERFORM IMS-STATUSCHECK                                              
075600     .                                                                    
075700     EJECT                                                                
075800                                                                          
075900 IMS-STATUSCHECK SECTION.                                                 
076000                                                                          
076100     SET STATUS-IX           TO 1                                         
076200     SEARCH GOOD-STATUS                                                   
076300       AT END                                                             
076400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
076500         DELIMITED BY SIZE INTO ERROR-TEXT                                
076600         CALL FELLOG                                                      
076700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
076800         CONTINUE                                                         
076900     END-SEARCH                                                           
077000     .                                                                    
