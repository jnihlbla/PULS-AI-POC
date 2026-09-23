000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W6016100.                                                 
000300 AUTHOR.        ERIK RINGQVIST / ARCHANA BHAT.                            
000400 DATE-WRITTEN.  MAJ 1984 / JUNE 2012.                                     
000500*    REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*        TP-PROGRAM FÖR REGISTRERING AV FÄLTET KDARTHNT                   
000800*        I SEGMENT 13 I ARTIKELREGISTRET.                                 
000900*                                                                         
001000*     +  STARTAR EV DISPATCH FÖR UPPDATERING AV URSPRUNG PÅ               
001100*        INLEVERANSREGISTRET.                                             
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W6T161                                              
001500*        MID:         W6I16101                                            
001600*    UTDATA.                                                              
001700*        MOD:         W6O16101                                            
001800*    SUBPROGRAM.                                                          
001900*        FELLOG                                                           
002000*        W006KOM  (DISPATCH)                                              
002100*    SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300*                                                                         
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77    PROGRAM-NAMN          PIC X(8)    VALUE 'W6016100'.                
003100 77    JA                    PIC X       VALUE 'J'.                       
003200 77    NEJ                   PIC X       VALUE 'N'.                       
003300*                                                                         
003400 77    MAX-RAD-IX-PLUS       PIC S9(9)   VALUE +10   COMP SYNC.           
003500 77    MAX-KVRADER           PIC S9(4)   VALUE +10   COMP SYNC.           
003600 77    RAD-IX                PIC S9(9)   VALUE +1    COMP SYNC.           
003700     SKIP3                                                                
003800*      --- VALID IDDC CODES                                               
003900*                                                                         
004000*01    -COPY WWDCKONS                                                     
004100       EJECT                                                              
004200 01    DYNAMISKA-SUBPROGRAM.                                              
004300   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI'.                 
004400   03  FELLOG                PIC X(8)    VALUE 'FELLOG'.                  
004500   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
004600   03  W6016110              PIC X(8)    VALUE 'W6016110'.                
004700   03  WL01MCNV              PIC X(8)    VALUE 'WL01MCNV'.                
004800****************************************************************          
004900 01  ALL-SPACE.                                                           
005000     03 FILLER               PIC X(50)   VALUE SPACE.                     
005100*                                                                         
005200 01  ALL-PLUS.                                                            
005300     03 FILLER               PIC X(50)   VALUE ALL '+'.                   
005400*                                                                         
005500*                                                                         
005600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
005700*                                                                         
005800 01    FILLER                PIC X(8)    VALUE 'MFS-WS  '.                
005900                                                                          
006000*01    MID -COPY W6I16101                                                 
006100     EJECT                                                                
006200*01    -COPY WMSGAREA                                                     
006300     EJECT                                                                
006400*  03    W6O16101 -COPY W6O16101         -RED MSG-AREA.                   
006500     EJECT                                                                
006600*01      -COPY WMFSAREA                                                   
006700*                                                                         
006800*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
006900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007000*01 -COPY WMSGINIT                                                        
007100     SKIP3                                                                
007200 01  FILLER                  PIC X(16)  VALUE 'REQU-AREA'.                
007300 01  REQU-AREA.                                                           
007400*    03 -COPY WZ01REQU                                                    
007500*    03 -COPY W60161I1                                                    
007600     EJECT                                                                
007700*                                                                         
007800 01  FILLER                  PIC X(16)  VALUE 'RESP-AREA'.                
007900 01  RESP-AREA.                                                           
008000*    03 -COPY WZ01RESP                                                    
008100*    03 -COPY W60161O1                                                    
008200*                                                                         
008300 01  FILLER                  PIC X(16)   VALUE 'WL01MCNV'.                
008400     SKIP3                                                                
008500*01  -COPY WL01MCNV                                                       
008600     EJECT                                                                
008700*                                                                         
008800     EJECT                                                                
008900*-----------------------------------------------------------              
009000 01    IMS-WS.                                                            
009100   03    FILLER              PIC X(8)    VALUE 'IMS-WS  '.                
009200                                                                          
009300*                            *** STATUSKOD FRÅN IMS                       
009400   03    STATUS-WS           PIC XX.                                      
009500     88    SEGMENT-FINNS               VALUE '  '.                        
009600     88    SEGMENT-SAKNAS              VALUE 'GE'.                        
009700                                                                          
009800   03    GODK-STATUSKODER.                                                
009900     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
010000                                                                          
010100*                            *** IMS FUNKTIONSKODER                       
010200*01      -COPY W0003                                                      
010300     EJECT                                                                
010400                                                                          
010500 LINKAGE SECTION.                                                         
010600*01      -COPY W0009     -PRE MSG-                                        
010700     EJECT                                                                
010800*01      -COPY W0009     -PRE DISP-                                       
010900     EJECT                                                                
011000*01      -COPY W0008     -PRE USEA-                                       
011100     05  FILLER                  PIC X.                                   
011200                                                                          
011300*01      -COPY W0008     -PRE ARTC-                                       
011400      05 FILLER              PIC X.                                       
011500     EJECT                                                                
011600*    PCB'ER FÖR SUBPGM                                                    
011700 01 KOM-KOMA-PCB         PIC X.                                           
011800     EJECT                                                                
011900*01      -COPY W0008     -PRE WDK7-                                       
012000      05 FILLER              PIC X.                                       
012100     EJECT                                                                
012110*01      -COPY W0008     -PRE WDT5-                                       
012120      05 FILLER              PIC X.                                       
012130     EJECT                                                                
012200 01  W005K7-WDB6-PCB     PIC X.                                           
012400 01  W005K7-WDK6-PCB     PIC X.                                           
012600 01  W005K7-WDK7-PCB     PIC X.                                           
012700     EJECT                                                                
012800 PROCEDURE DIVISION USING MSG-PCB DISP-PCB USEA-PCB                       
012900                          ARTC-PCB KOM-KOMA-PCB WDK7-PCB                  
012910                          WDT5-PCB W005K7-WDB6-PCB                        
013000                          W005K7-WDK6-PCB W005K7-WDK7-PCB.                
013200     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB USEA-PCB                      
013300                           ARTC-PCB KOM-KOMA-PCB WDK7-PCB                 
013310                           WDT5-PCB W005K7-WDB6-PCB                       
013320                           W005K7-WDK6-PCB W005K7-WDK7-PCB.               
013600 STYR SECTION.                                                            
013700     PERFORM IMS-GET-MSG                                                  
013800     IF SEGMENT-FINNS                                                     
013900       PERFORM A-INIT-SPARA-INPUT                                         
014000       IF MFS-UPDATE                                                      
014100          SET REQU-UPDATE TO TRUE                                         
014200       END-IF                                                             
014300       IF MFS-IDTRANS = '6161'                                            
014400         PERFORM F-CALL-BIZ-LOGIC-W6016110                                
014500       ELSE                                                               
014600         PERFORM MFS-RENSA-FAELT-MODTAB                                   
014700       END-IF                                                             
014800                                                                          
014900       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O16101 + 4                      
015000       PERFORM IMS-ISRT-MSG                                               
015100     END-IF                                                               
015200                                                                          
015300     MOVE ZERO TO RETURN-CODE                                             
015400     GOBACK                                                               
015500     .                                                                    
015600     EJECT                                                                
015700 A-INIT-SPARA-INPUT SECTION.                                              
015800                                                                          
015900     IF MSG-DUBBLA-TRANSKODER                                             
016000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I16101                 
016100       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
016200       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
016300                                             REQU-KDMFSFOR                
016400     ELSE                                                                 
016500       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I16101                 
016600       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
016700       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
016800                                             REQU-KDMFSFOR                
016900     END-IF                                                               
017000                                                                          
017100     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
017200     MOVE MSG-SIGNON-USERID               TO REQU-IDUSER                  
017300                                                                          
017400     PERFORM AA-INIT-NYCKLAR                                              
017500                                                                          
017600     MOVE LOW-VALUE       TO MSG-AREA                                     
017700     MOVE 'W6O161N1'      TO MFS-IDMOD                                    
017800     MOVE '6161'          TO MOD-IDTRANS                                  
017900     MOVE MFS-RENSA-FAELT TO MOD-MESSAGE-RAD1                             
018000                             MOD-MESSAGE-RAD23                            
018100     .                                                                    
018200     EJECT                                                                
018300 AA-INIT-NYCKLAR SECTION.                                                 
018400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
018500     MOVE '001'   TO MSGI-KDCALL                                          
018600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
018700     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
018800     MOVE '6161'                 TO MSGI-IDTRANS                          
018900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
019000     MOVE MSGI-IDDC                       TO REQU-IDDC                    
019100     .                                                                    
019200     EJECT                                                                
019300 F-CALL-BIZ-LOGIC-W6016110 SECTION.                                       
019400                                                                          
019500     PERFORM FA-INIT-REQU                                                 
019600     CALL W6016110 USING REQU-AREA RESP-AREA MAX-KVRADER                  
019700                         MSG-PCB DISP-PCB ARTC-PCB KOM-KOMA-PCB           
019800                         WDK7-PCB WDT5-PCB W005K7-WDB6-PCB                
019900                         W005K7-WDK6-PCB W005K7-WDK7-PCB                  
020000     PERFORM FB-SET-MSG-AND-HILIGHT                                       
020100     PERFORM FC-MOVE-RESP-TO-MOD                                          
020200     .                                                                    
020300     EJECT                                                                
020400 FA-INIT-REQU SECTION.                                                    
020500                                                                          
020600     MOVE MAX-KVRADER                 TO REQU-KVRADER                     
020700                                                                          
020800     MOVE +1                          TO RAD-IX                           
020900     PERFORM UNTIL RAD-IX > MAX-RAD-IX-PLUS                               
021000       IF MID-IDARTNR(RAD-IX) = ALL '+'                                   
021100          MOVE ALL-PLUS               TO REQU-IDARTNR(RAD-IX)             
021200       ELSE                                                               
021300          MOVE MID-IDARTNR(RAD-IX)    TO REQU-IDARTNR(RAD-IX)             
021400       END-IF                                                             
021500                                                                          
021600       IF MID-KDARTHNT-V(RAD-IX) = ALL '+'                                
021700          MOVE ALL-PLUS               TO REQU-KDARTHNT-V(RAD-IX)          
021800       ELSE                                                               
021900          MOVE MID-KDARTHNT-V(RAD-IX) TO REQU-KDARTHNT-V(RAD-IX)          
022000       END-IF                                                             
022100                                                                          
022200       IF MID-KDARTHNT-H(RAD-IX) = ALL '+'                                
022300          MOVE ALL-PLUS               TO REQU-KDARTHNT-H(RAD-IX)          
022400       ELSE                                                               
022500          MOVE MID-KDARTHNT-H(RAD-IX) TO REQU-KDARTHNT-H(RAD-IX)          
022600       END-IF                                                             
022700                                                                          
022800       IF MID-KDARTURS(RAD-IX) = ALL '+'                                  
022900          MOVE ALL-PLUS               TO REQU-KDARTURS(RAD-IX)            
023000       ELSE                                                               
023100          MOVE MID-KDARTURS(RAD-IX)   TO REQU-KDARTURS(RAD-IX)            
023200       END-IF                                                             
023300       ADD +1                         TO RAD-IX                           
023400     END-PERFORM                                                          
023500                                                                          
023600     MOVE '101'                       TO REQU-IDMSGVER                    
023700     .                                                                    
023800 FB-SET-MSG-AND-HILIGHT SECTION.                                          
023900                                                                          
024000     MOVE RESP-IDMSG-ERROR          TO MCNV-IDMSG-ERROR                   
024100     MOVE RESP-IDMSG-INFO           TO MCNV-IDMSG-INFO                    
024200     MOVE RESP-IDELMT-ERROR         TO MCNV-IDELMT-ERROR                  
024300     IF REQU-KDMFSFOR = '1'                                               
024400        MOVE 'SV'                   TO MCNV-IDSPRAK                       
024500     ELSE                                                                 
024600        MOVE 'EN'                   TO MCNV-IDSPRAK                       
024700     END-IF                                                               
024800     CALL WL01MCNV USING MCNV-AREA                                        
024900                                                                          
025000     MOVE MCNV-MFSFEL               TO MOD-MESSAGE-RAD1                   
025100     MOVE MCNV-MFSINF               TO MOD-MESSAGE-RAD23                  
025200     .                                                                    
025300     EJECT                                                                
025400 FC-MOVE-RESP-TO-MOD SECTION.                                             
025500                                                                          
025600     MOVE +1                             TO RAD-IX                        
025700                                                                          
025800     PERFORM UNTIL RAD-IX > RESP-KVRADER                                  
025900       MOVE RESP-IDARTNR-ATTR(RAD-IX)    TO                               
026000                                   MOD-IDARTNR-ATTR(RAD-IX)               
026100       MOVE RESP-KDARTHNT-V-ATTR(RAD-IX) TO                               
026200                                   MOD-KDARTHNT-V-ATTR(RAD-IX)            
026300       MOVE RESP-KDARTHNT-H-ATTR(RAD-IX) TO                               
026400                                   MOD-KDARTHNT-H-ATTR(RAD-IX)            
026500       MOVE RESP-KDARTURS-ATTR(RAD-IX)   TO                               
026600                                   MOD-KDARTURS-ATTR(RAD-IX)              
026700       IF RESP-IDARTNR(RAD-IX) = SPACE                                    
026800          MOVE MFS-ERASE-FIELD           TO MOD-IDARTNR(RAD-IX)           
026900       ELSE                                                               
027000         IF RESP-IDARTNR(RAD-IX) = ALL '+'                                
027100            MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDARTNR(RAD-IX)           
027200         ELSE                                                             
027300            MOVE RESP-IDARTNR(RAD-IX)    TO MOD-IDARTNR(RAD-IX)           
027400         END-IF                                                           
027500       END-IF                                                             
027600                                                                          
027700       IF RESP-KDARTURS(RAD-IX) = SPACE                                   
027800          MOVE MFS-ERASE-FIELD           TO MOD-KDARTURS(RAD-IX)          
027900       ELSE                                                               
028000         IF RESP-KDARTURS(RAD-IX) = ALL '+'                               
028100            MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KDARTURS(RAD-IX)          
028200         ELSE                                                             
028300            MOVE RESP-KDARTURS(RAD-IX)   TO MOD-KDARTURS(RAD-IX)          
028400         END-IF                                                           
028500       END-IF                                                             
028600                                                                          
028700       IF RESP-KDARTHNT-V(RAD-IX) = SPACE                                 
028800          MOVE MFS-ERASE-FIELD           TO MOD-KDARTHNT-V(RAD-IX)        
028900       ELSE                                                               
029000         IF RESP-KDARTHNT-V(RAD-IX) = ALL '+'                             
029100            MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KDARTHNT-V(RAD-IX)        
029200         ELSE                                                             
029300            MOVE RESP-KDARTHNT-V(RAD-IX) TO MOD-KDARTHNT-V(RAD-IX)        
029400         END-IF                                                           
029500       END-IF                                                             
029600                                                                          
029700       IF RESP-KDARTHNT-H(RAD-IX) = SPACE                                 
029800          MOVE MFS-ERASE-FIELD           TO MOD-KDARTHNT-H(RAD-IX)        
029900       ELSE                                                               
030000         IF RESP-KDARTHNT-H(RAD-IX) = ALL '+'                             
030100            MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KDARTHNT-H(RAD-IX)        
030200         ELSE                                                             
030300            MOVE RESP-KDARTHNT-H(RAD-IX) TO MOD-KDARTHNT-H(RAD-IX)        
030400         END-IF                                                           
030500       END-IF                                                             
030600                                                                          
030700       ADD +1                            TO RAD-IX                        
030800     END-PERFORM                                                          
030900     .                                                                    
031000                                                                          
031100 MFS-RENSA-FAELT-MODTAB      SECTION.                                     
031200                                                                          
031300     MOVE +1 TO RAD-IX                                                    
031400     PERFORM  UNTIL RAD-IX > MAX-RAD-IX-PLUS                              
031500       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR    (RAD-IX)                    
031600                               MOD-KDARTHNT-V (RAD-IX)                    
031700                               MOD-KDARTHNT-H (RAD-IX)                    
031800                               MOD-KDARTURS   (RAD-IX)                    
031900       ADD +1 TO RAD-IX                                                   
032000     END-PERFORM                                                          
032100     EJECT                                                                
032200     .                                                                    
032300* IMS SECTIONER                                                           
032400 IMS-GET-MSG SECTION.                                                     
032500                                                                          
032600     MOVE '  QC' TO GODK-STATUSKODER                                      
032700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
032800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032900     PERFORM IMS-STATUS-KONTROLL                                          
033000     SKIP3                                                                
033100     .                                                                    
033200 IMS-ISRT-MSG SECTION.                                                    
033300                                                                          
033400     IF NOT ENGLISH-TEXT                                                  
033500       MOVE '0' TO MFS-KDHUVOMR                                           
033600     END-IF                                                               
033700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
033800     MOVE SPACE TO GODK-STATUSKODER                                       
033900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
034000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034100     PERFORM IMS-STATUS-KONTROLL                                          
034200     .                                                                    
034300     EJECT                                                                
034400 IMS-STATUS-KONTROLL SECTION.                                             
034500     SET STATUS-IX TO 1                                                   
034600     SEARCH GODK-STATUS AT END CALL FELLOG                                
034700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
034800     END-SEARCH                                                           
035000     .                                                                    
