000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4034300.                                                
000400 AUTHOR.         GUNNAR L, IDK.                                           
000500     DATE-WRITTEN.   MARS 1981.                                           
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        FLYTTA RAD I PACKAT KOLLI                                        
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T343                                              
001400*        MID:         W4I34301                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O34301                                            
001800*    SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP3                                                                
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400*    -- CHECKED BY WY2000                                                 
002500     SKIP3                                                                
002600 77   PROGRAM-NAMN           VALUE 'W4034300'                             
002700                                 PIC X(8).                                
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FILLER                      PIC X(08) VALUE 'ERRORTEX'.              
003100 77  FELTEXT                     PIC X(64) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  WS-MSGI-IDLAND-SPR          PIC X(2)    VALUE SPACE.                 
003600       EJECT                                                              
003700*                                                                         
003800 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003900 77  MAX-KVRADER                 PIC S9(4)   VALUE +16  COMP SYNC.        
004000 77  MAX-MODLAENGD               PIC S9(4)   VALUE +830 COMP SYNC.        
004010 77  WS-KDTRANS                  PIC X(6)    VALUE 'W4T343'.              
004100 77  WS-IDTRANS                  PIC X(4).                                
004200     88  WS-GODKAEND-BILD                    VALUE '4343'.                
004201                                                                          
004300*                                                                         
004400 01  ALL-SPACE.                                                           
004500     03  FILLER                  PIC X(80)  VALUE SPACE.                  
004600 01  ALL-PLUS.                                                            
004700     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
004800*                                                                         
004900*                                                                         
005000 01  DYNAMISKA-SUBPROGRAM.                                                
005100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005400     03  W4034310                PIC X(8)    VALUE 'W4034310'.            
005500     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
005600     SKIP2                                                                
005700 01  FILLER                      PIC X(16)  VALUE 'W005INIT '.            
005800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
005900*01 -COPY WMSGINIT                                                        
006000     SKIP2                                                                
006100 01  FILLER                      PIC X(16)  VALUE 'WL01MCNV '.            
006200*01 -COPY WL01MCNV                                                        
006300     SKIP2                                                                
006400 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
006500 01  REQU-AREA.                                                           
006600*    03 -COPY WZ01REQ2                                                    
006700*    03 -COPY W40343I2                                                    
006800     EJECT                                                                
006900 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
007000 01  RESP-AREA.                                                           
007100*    03 -COPY WZ01RES2                                                    
007200*    03 -COPY W40343O1                                                    
007300     EJECT                                                                
007400                                                                          
007500*                                                                         
007600 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
007700     SKIP3                                                                
007800 01  SWITCHAR.                                                            
007900*                                                                         
008000     03  SAMMA-NYCKLAR-SW        PIC X(1)    VALUE 'N'.                   
008100       88  SAMMA-NYCKLAR                     VALUE 'J'.                   
008200*                                                                         
008300     03  UPPDAT-TILLAATEN-SW     PIC X(1)    VALUE 'N'.                   
008400       88  UPPDAT-TILLAATEN                  VALUE 'J'.                   
008500*                                                                         
008600     EJECT                                                                
008700******************************************************************        
008800*                                                                         
008900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
009200     SKIP3                                                                
009300*01  MID -COPY W4I34301 -PRE MID-.                                        
009400     EJECT                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700*  03  MOD -COPY W4O34301 -PRE MOD- -RED MSG-AREA.                        
009800     EJECT                                                                
009900*01  -COPY WMFSAREA                                                       
010000     EJECT                                                                
010100******************************************************************        
010200*                                                                         
010300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010400*                                                                         
010500 01  IMS-WS.                                                              
010600   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
010700     SKIP3                                                                
010800*                        **** STATUS-KOD FRÅN IMS                         
010900   03  STATUS-WS                 PIC X(2).                                
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011300     88  BASEN-SLUT                          VALUE 'GB'.                  
011400     SKIP3                                                                
011500   03  GODK-STATUSKODER.                                                  
011600     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011700     SKIP3                                                                
011800 01    SSA1                      PIC X(192).                              
011900 01    SSA2                      PIC X(64).                               
012000 01    SSA3                      PIC X(64).                               
012100     EJECT                                                                
012200*                            IMS FUNKTIONSKODER                           
012300*01    -COPY W0003                                                        
012400     SKIP3                                                                
012500                                                                          
012600     EJECT                                                                
012700                                                                          
012800 LINKAGE SECTION.                                                         
012900     SKIP3                                                                
013000*01  -COPY W0009     -PRE MSG-                                            
013100     EJECT                                                                
013200 01  TMS-CRE-PCB                 PIC X.                                   
013210 01  TMS-DEL-PCB                 PIC X.                                   
013300 01  ATAB-PCB                    PIC X.                                   
013400     EJECT                                                                
013500*01  -COPY W0008     -PRE WDP7-                                           
013600     05  FILLER                  PIC X.                                   
013700     EJECT                                                                
013800*01  -COPY W0008     -PRE WDE6-                                           
013900     05  FILLER                  PIC X(1).                                
014000     SKIP3                                                                
014100*01  -COPY W0008     -PRE WDE4FSEQ-                                       
014200     05  FILLER                  PIC X(1).                                
014300     SKIP3                                                                
014400*01  -COPY W0008     -PRE WDE4-                                           
014500     05  FILLER                  PIC X(1).                                
014600     EJECT                                                                
014700*01  -COPY W0008     -PRE WDE4F-                                          
014800     05  FILLER                  PIC X(1).                                
014900     EJECT                                                                
015000*01  -COPY W0008     -PRE WDQ5A-                                          
015100     05  FILLER                  PIC X(1).                                
015200     EJECT                                                                
015300*01  -COPY W0008     -PRE WDB6-                                           
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600*01  -COPY W0008     -PRE PLATS-DM-                                       
015700     05  FILLER                  PIC X.                                   
015800     EJECT                                                                
015900*01  -COPY W0008     -PRE PLATS-DN-                                       
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016200*01  -COPY W0008     -PRE PLATS-DP-                                       
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008     -PRE PLATS-DO-                                       
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008     -PRE PLATS-WDE6C-                                    
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100*01  -COPY W0008     -PRE PLATS-GMTC-                                     
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017400*01  -COPY W0008     -PRE PLATS-WDB6-                                     
017500     05  FILLER                  PIC X.                                   
017600     EJECT                                                                
017700 01  DNOT-ORQP-PCB               PIC X.                                   
017800 01  DNOT-ORQP2-PCB              PIC X.                                   
017900 01  DNOT-ORQP3-PCB              PIC X.                                   
018000 01  DNOT-4013-PCB               PIC X.                                   
018100 01  DNOT-BENA-PCB               PIC X.                                   
018200     EJECT                                                                
018300 01  TMS-1165-PCB                PIC X.                                   
018400 01  TMS-4141-PCB                PIC X.                                   
018500 01  TMS-WDB2-PCB                PIC X.                                   
018600 01  TMS-WDB6-PCB                PIC X.                                   
018700 01  TMS-WDD3-PCB                PIC X.                                   
018800 01  TMS-WDB1-PCB                PIC X.                                   
018900 01  TMS-WDE4A-PCB               PIC X.                                   
019000 01  TMS-WDE4F-PCB               PIC X.                                   
019100 01  TMS-WDQ2-PCB                PIC X.                                   
019200 01  TMS-WDQ3-PCB                PIC X.                                   
019300 01  TMS-WDK6-PCB                PIC X.                                   
019400 01  TMS-WDE6-PCB                PIC X.                                   
019500 01  TMS-WDK5-PCB                PIC X.                                   
019510 01  TMS-WDQ2C-PCB               PIC X.                                   
019600     EJECT                                                                
019700*01  -COPY W0008     -PRE XXJK-                                           
019800     05  FILLER                  PIC X.                                   
019900                                                                          
020000*01  -COPY W0008  -PRE WDK5-                                              
020100     05  FILLER                  PIC X.                                   
020200     EJECT                                                                
020300                                                                          
020400 PROCEDURE DIVISION USING MSG-PCB TMS-CRE-PCB TMS-DEL-PCB                 
020410                 ATAB-PCB WDP7-PCB                                        
020500                 WDE6-PCB  WDE4FSEQ-PCB WDE4-PCB WDE4F-PCB                
020600                 WDQ5A-PCB WDB6-PCB                                       
020700                 PLATS-DM-PCB PLATS-DN-PCB                                
020800                 PLATS-DP-PCB PLATS-DO-PCB                                
020900                 PLATS-WDE6C-PCB PLATS-GMTC-PCB                           
021000                 PLATS-WDB6-PCB                                           
021100                 DNOT-ORQP-PCB                                            
021200                 DNOT-ORQP2-PCB                                           
021300                 DNOT-ORQP3-PCB                                           
021400                 DNOT-4013-PCB                                            
021500                 DNOT-BENA-PCB                                            
021600                 TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                   
021700                 TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                   
021800                 TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                 
021900                 TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                   
022000                 TMS-WDK5-PCB TMS-WDQ2C-PCB                               
022100                 XXJK-PCB                                                 
022200                 WDK5-PCB.                                                
022300 MAIN SECTION.                                                            
022400     ENTRY 'DLITCBL' USING MSG-PCB TMS-CRE-PCB TMS-DEL-PCB                
022410                 ATAB-PCB WDP7-PCB                                        
022500                 WDE6-PCB  WDE4FSEQ-PCB WDE4-PCB WDE4F-PCB                
022600                 WDQ5A-PCB WDB6-PCB                                       
022700                 PLATS-DM-PCB PLATS-DN-PCB                                
022800                 PLATS-DP-PCB PLATS-DO-PCB                                
022900                 PLATS-WDE6C-PCB PLATS-GMTC-PCB                           
023000                 PLATS-WDB6-PCB                                           
023100                 DNOT-ORQP-PCB                                            
023200                 DNOT-ORQP2-PCB                                           
023300                 DNOT-ORQP3-PCB                                           
023400                 DNOT-4013-PCB                                            
023500                 DNOT-BENA-PCB                                            
023600                 TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                   
023700                 TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                   
023800                 TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                 
023900                 TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                   
024000                 TMS-WDK5-PCB TMS-WDQ2C-PCB                               
024100                 XXJK-PCB                                                 
024200                 WDK5-PCB.                                                
024300                                                                          
024400******************************************************************        
024500*                                                                *        
024600*WDE4FSEQ-PCB                                                    *        
024700*  & WDE6-PCB  : ANVÄNDS VID LÄSNINGAR OCH                       *        
024800*                UPPDATERAR KOLLIT SOM MAN FLYTTAR FRÅN          *        
024900*                                                                *        
025000*    WDE4-PCB  : UPPDATERAR KOLLIT SOM MAN FLYTTAR TILL          *        
025100*  & WDE6                                                        *        
025200******************************************************************        
025300                                                                          
025400     PERFORM IMS-GET-MSG                                                  
025500     IF SEGMENT-FINNS                                                     
025600       PERFORM A-INIT                                                     
025700       PERFORM B-INIT-KEYS                                                
025800       PERFORM C-INIT-REQU                                                
025900       IF UPPDAT-TILLAATEN                                                
026000         SET REQU-UPDATE     TO TRUE                                      
026100       ELSE                                                               
026200         SET REQU-QUERY      TO TRUE                                      
026300       END-IF                                                             
026400                                                                          
026500       PERFORM F-CALL-BIZ-LOGIC-W4034310                                  
026600       MOVE MAX-MODLAENGD    TO MSG-KVLL                                  
026700       PERFORM IMS-INSERT-MSG                                             
026800     END-IF                                                               
026900     MOVE ZERO TO RETURN-CODE                                             
027000     GOBACK                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 A-INIT SECTION.                                                          
027400                                                                          
027500     IF MSG-DUBBLA-TRANSKODER                                             
027600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I34301                 
027700       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
027800       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
027900     ELSE                                                                 
028000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I34301                  
028100       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
028200       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
028300     END-IF                                                               
028400     MOVE MFS-IDTRANS            TO WS-IDTRANS                            
028500                                                                          
028600     MOVE ALL '+'                TO MSGI-WMSGINIT                         
028700     MOVE '001'                  TO MSGI-KDCALL                           
028800     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
028900     MOVE '4343'                 TO MSGI-IDTRANS                          
029000     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
029100     CALL W005INIT            USING MSGI-WMSGINIT WDP7-PCB                
029200     MOVE MSGI-IDLAND-SPR        TO WS-MSGI-IDLAND-SPR                    
029300     MOVE MSGI-IDSPRAK           TO MCNV-IDSPRAK                          
029400                                    REQU-IDSPRAK                          
029500     MOVE MSGI-IDUSER            TO REQU-IDUSER                           
029600     MOVE MSGI-KDMATT            TO REQU-KDMATT                           
029700                                                                          
029800     IF MSGI-IDLAND-SPR = 'GB'                                            
029900       MOVE '2'                  TO MFS-KDMFSFOR                          
030000     ELSE                                                                 
030100       MOVE '1'                  TO MFS-KDMFSFOR                          
030200     END-IF                                                               
030300                                                                          
030400*    -- COMMING FROM CLASSIC IMS                                          
030500     MOVE '101'                  TO REQU-IDRESVER                         
030600                                                                          
030700     MOVE LOW-VALUE              TO MSG-AREA                              
030800     MOVE 'W4O343N1'             TO MFS-IDMOD                             
030900     MOVE '4343'                 TO MOD-TRANS-NUMMER                      
031000     .                                                                    
031100     EJECT                                                                
031200                                                                          
031300 B-INIT-KEYS SECTION.                                                     
031400                                                                          
031500     IF MID-IDPRODNR-IN = ALL '+'                                         
031600       MOVE MID-IDPRODNR-UT      TO REQU-IDPRODNR-KEY                     
031700       INSPECT REQU-IDPRODNR-KEY                                          
031800                  REPLACING LEADING SPACE BY ZERO                         
031900       IF MID-IDKOLLI-IN = ALL '+'                                        
032000         MOVE MID-IDKOLLI-UT     TO REQU-IDKOLLI-KEY                      
032100         INSPECT REQU-IDKOLLI-KEY                                         
032200                  REPLACING LEADING SPACE BY ZERO                         
032300         MOVE JA                 TO SAMMA-NYCKLAR-SW                      
032400       ELSE                                                               
032500         MOVE MID-IDKOLLI-IN     TO REQU-IDKOLLI-KEY                      
032600       END-IF                                                             
032700     ELSE                                                                 
032800       MOVE MID-IDPRODNR-IN      TO REQU-IDPRODNR-KEY                     
032900       IF MID-IDKOLLI-IN = ALL '+'                                        
033000         MOVE SPACE              TO REQU-IDKOLLI-KEY                      
033100       ELSE                                                               
033200         MOVE MID-IDKOLLI-IN     TO REQU-IDKOLLI-KEY                      
033300       END-IF                                                             
033400     END-IF                                                               
033500                                                                          
033600     MOVE MSGI-IDDC              TO REQU-IDDC-KEY                         
033700                                                                          
033800     IF  MFS-IDTRANS = '4343' AND                                         
033900         NOT MSG-DUBBLA-TRANSKODER AND                                    
034000         SAMMA-NYCKLAR AND                                                
034100         REQU-IDPRODNR-KEY > ZERO AND                                     
034200         MID-IDRADNR-FOM  NUMERIC AND                                     
034300         MID-IDRADNR-FOM  > ZERO  AND                                     
034400         MID-IDRADNR-TOM  NUMERIC AND                                     
034500         MID-IDRADNR-TOM  > ZERO                                          
034600         MOVE JA                 TO UPPDAT-TILLAATEN-SW                   
034700     END-IF                                                               
034800                                                                          
034900     MOVE REQU-IDPRODNR-KEY      TO MOD-IDPRODNR-UT                       
035000     MOVE REQU-IDKOLLI-KEY       TO MOD-IDKOLLI-UT                        
035100     MOVE MSGI-IDDC              TO MOD-IDDC-UT                           
035200                                                                          
035300     IF WS-GODKAEND-BILD                                                  
035400       INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE            
035500       INSPECT MOD-IDKOLLI-UT  REPLACING LEADING ZERO BY SPACE            
035600     END-IF                                                               
035700                                                                          
035800     MOVE MFS-RENSA-FAELT        TO MOD-IDPRODNR-IN                       
035900                                    MOD-IDKOLLI-IN                        
036000                                    MOD-IDDC-IN                           
036100     MOVE SPACE                  TO MOD-MESSAGE-RAD1                      
036200                                    MOD-MESSAGE-RAD23                     
036300                                                                          
036400     .                                                                    
036500     EJECT                                                                
036600 C-INIT-REQU SECTION.                                                     
036700                                                                          
036800     MOVE MAX-KVRADER            TO REQU-KVRADER                          
036900     IF MID-IDRADNR-FOM = ALL '+'                                         
037000       MOVE ALL-PLUS             TO REQU-IDRADNR-FOM                      
037100     ELSE                                                                 
037200       MOVE MID-IDRADNR-FOM      TO REQU-IDRADNR-FOM                      
037300     END-IF                                                               
037400                                                                          
037500     IF MID-IDRADNR-TOM = ALL '+'                                         
037600       MOVE ALL-PLUS             TO REQU-IDRADNR-TOM                      
037700     ELSE                                                                 
037800       MOVE MID-IDRADNR-TOM      TO REQU-IDRADNR-TOM                      
037900     END-IF                                                               
038000                                                                          
038100     MOVE MID-FLJANEJ-ALLA       TO REQU-FLJANEJ-ALLA                     
038200     MOVE MID-IDKOLLI-ALLA       TO REQU-IDKOLLI-ALLA                     
038300     MOVE MID-IDKOLLI-NY         TO REQU-IDKOLLI-NY                       
038400     MOVE MID-KDKOLLI            TO REQU-KDKOLLI                          
038500     MOVE MID-KDEMBTYP           TO REQU-KDEMBTYP                         
038600     MOVE MID-VKORDBTO           TO REQU-VKORDBTO                         
038700     MOVE MID-DIKOLLIL           TO REQU-DIKOLLIL                         
038800     MOVE MID-DIKOLLIB           TO REQU-DIKOLLIB                         
038900     MOVE MID-DIKOLLIH           TO REQU-DIKOLLIH                         
039000                                                                          
039100     MOVE +1 TO INDX                                                      
039200     PERFORM UNTIL INDX > MAX-KVRADER                                     
039300       MOVE MID-FLNOLLAD (INDX)  TO REQU-FLNOLLAD-LINE (INDX)             
039400       MOVE MID-IDARTNR (INDX)   TO REQU-IDARTNR-LINE (INDX)              
039500       MOVE MID-IDKOLLI-RAD (INDX)                                        
039600                                 TO REQU-IDKOLLI-LINE (INDX)              
039700       MOVE MID-KVLEVART (INDX)  TO REQU-KVLEVART-LINE-IN (INDX)          
039800       ADD  +1 TO INDX                                                    
039900     END-PERFORM                                                          
040000                                                                          
040100     .                                                                    
040200     EJECT                                                                
040300 F-CALL-BIZ-LOGIC-W4034310 SECTION.                                       
040400                                                                          
040500     CALL W4034310 USING REQU-AREA RESP-AREA                              
040510                         MAX-KVRADER WS-KDTRANS                           
040600                         MSG-PCB  TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB        
040700                         WDP7-PCB  WDE6-PCB  WDE4FSEQ-PCB WDE4-PCB        
040800                         WDE4F-PCB WDQ5A-PCB WDB6-PCB                     
040900                         PLATS-DM-PCB PLATS-DN-PCB                        
041000                         PLATS-DP-PCB PLATS-DO-PCB                        
041100                         PLATS-WDE6C-PCB PLATS-GMTC-PCB                   
041200                         PLATS-WDB6-PCB                                   
041300                         DNOT-ORQP-PCB                                    
041400                         DNOT-ORQP2-PCB                                   
041500                         DNOT-ORQP3-PCB                                   
041600                         DNOT-4013-PCB                                    
041700                         DNOT-BENA-PCB                                    
041800                         TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB           
041900                         TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB           
042000                         TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB         
042100                         TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB           
042200                         TMS-WDK5-PCB TMS-WDQ2C-PCB                       
042300                         XXJK-PCB                                         
042400                         WDK5-PCB                                         
042500                                                                          
042600     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
042700        RESP-IDMSG-INFO  NOT = SPACE                                      
042800       PERFORM FA-SET-MSG-AND-HILIGHT                                     
042900     END-IF                                                               
043000     PERFORM FB-MOVE-RESP-TO-MOD                                          
043100     .                                                                    
043200     EJECT                                                                
043300                                                                          
043400 FA-SET-MSG-AND-HILIGHT SECTION.                                          
043500                                                                          
043600     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
043700     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
043800     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
043900                                                                          
044000     CALL WL01MCNV            USING MCNV-AREA                             
044100     IF RESP-IDMSG-INFO = '403'                                           
044200       STRING MCNV-MFSINF DELIMITED BY ':' ': ' RESP-ARB-ADDRESS          
044300             DELIMITED BY SIZE INTO MOD-MESSAGE-RAD23                     
044400     ELSE                                                                 
044500       MOVE MCNV-MFSINF          TO MOD-MESSAGE-RAD23                     
044600     END-IF                                                               
044700     MOVE MCNV-MFSFEL            TO MOD-MESSAGE-RAD1                      
044800                                                                          
044900     .                                                                    
045000     EJECT                                                                
045100                                                                          
045200 FB-MOVE-RESP-TO-MOD SECTION.                                             
045300                                                                          
045400     IF RESP-IDRADNR-FOM = SPACE                                          
045500       MOVE MFS-RENSA-FAELT      TO MOD-IDRADNR-FOM                       
045600     ELSE                                                                 
045700       IF RESP-IDRADNR-FOM = ALL '+'                                      
045800         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDRADNR-FOM                       
045900       ELSE                                                               
046000         MOVE RESP-IDRADNR-FOM   TO MOD-IDRADNR-FOM                       
046100       END-IF                                                             
046200     END-IF                                                               
046300                                                                          
046400     IF RESP-IDRADNR-TOM = SPACE                                          
046500       MOVE MFS-RENSA-FAELT      TO MOD-IDRADNR-TOM                       
046600     ELSE                                                                 
046700       IF RESP-IDRADNR-TOM = ALL '+'                                      
046800         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDRADNR-TOM                       
046900       ELSE                                                               
047000         MOVE RESP-IDRADNR-TOM   TO MOD-IDRADNR-TOM                       
047100       END-IF                                                             
047200     END-IF                                                               
047300                                                                          
047400     IF RESP-ORDER-INFO = SPACE                                           
047500       MOVE MFS-RENSA-FAELT      TO MOD-ORDER-INFO                        
047600     ELSE                                                                 
047700       IF RESP-ORDER-INFO = ALL '+'                                       
047800         MOVE MFS-ROER-EJ-FAELT  TO MOD-ORDER-INFO                        
047900       ELSE                                                               
048000         MOVE RESP-IDDISTR       TO MOD-IDDISTR                           
048100         MOVE RESP-IDKUNDNR      TO MOD-IDKUNDNR                          
048200         MOVE RESP-IDDC          TO MOD-IDDC                              
048300         MOVE RESP-KDORDKL       TO MOD-KDORDKL                           
048400         MOVE SPACE              TO MOD-FILLERINF1                        
048500                                    MOD-FILLERINF2                        
048600       END-IF                                                             
048700     END-IF                                                               
048800                                                                          
048900     IF RESP-KVORDRAD = SPACE                                             
049000       MOVE MFS-RENSA-FAELT      TO MOD-KVORDRAD                          
049100     ELSE                                                                 
049200       IF RESP-KVORDRAD = ALL '+'                                         
049300         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVORDRAD                          
049400       ELSE                                                               
049500         MOVE RESP-KVORDRAD      TO MOD-KVORDRAD                          
049600       END-IF                                                             
049700     END-IF                                                               
049800                                                                          
049900     IF RESP-TIPACKN = SPACE                                              
050000       MOVE MFS-RENSA-FAELT      TO MOD-TIPACKN                           
050100     ELSE                                                                 
050200       IF RESP-TIPACKN = ALL '+'                                          
050300         MOVE MFS-ROER-EJ-FAELT  TO MOD-TIPACKN                           
050400       ELSE                                                               
050500         MOVE RESP-TIPACKN       TO MOD-TIPACKN                           
050600       END-IF                                                             
050700     END-IF                                                               
050800                                                                          
050900     MOVE RESP-FLJANEJ-ALLA-ATTR TO MOD-FLJANEJ-ALLA-ATTR                 
051000     IF RESP-FLJANEJ-ALLA = SPACE                                         
051100       MOVE MFS-RENSA-FAELT      TO MOD-FLJANEJ-ALLA                      
051200     ELSE                                                                 
051300       IF RESP-FLJANEJ-ALLA = ALL '+'                                     
051400         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLJANEJ-ALLA                      
051500       ELSE                                                               
051600         MOVE RESP-FLJANEJ-ALLA  TO MOD-FLJANEJ-ALLA                      
051700       END-IF                                                             
051800     END-IF                                                               
051900                                                                          
052000     IF RESP-KDSVAR-ALLA = SPACE                                          
052100       MOVE MFS-RENSA-FAELT      TO MOD-KDSVAR-ALLA                       
052200     ELSE                                                                 
052300       IF RESP-KDSVAR-ALLA = ALL '+'                                      
052400         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDSVAR-ALLA                       
052500       ELSE                                                               
052600         MOVE RESP-KDSVAR-ALLA   TO MOD-KDSVAR-ALLA                       
052700       END-IF                                                             
052800     END-IF                                                               
052900                                                                          
053000     MOVE RESP-IDKOLLI-ALLA-ATTR TO MOD-IDKOLLI-ALLA-ATTR                 
053100     IF RESP-IDKOLLI-ALLA = SPACE                                         
053200       MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI-ALLA                      
053300     ELSE                                                                 
053400       IF RESP-IDKOLLI-ALLA = ALL '+'                                     
053500         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKOLLI-ALLA                      
053600       ELSE                                                               
053700         MOVE RESP-IDKOLLI-ALLA  TO MOD-IDKOLLI-ALLA                      
053800       END-IF                                                             
053900     END-IF                                                               
054000                                                                          
054100     MOVE RESP-IDKOLLI-NY-ATTR   TO MOD-IDKOLLI-NY-ATTR                   
054200     IF RESP-IDKOLLI-NY = SPACE                                           
054300       MOVE MFS-RENSA-FAELT      TO MOD-IDKOLLI-NY                        
054400     ELSE                                                                 
054500       IF RESP-IDKOLLI-NY = ALL '+'                                       
054600         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDKOLLI-NY                        
054700       ELSE                                                               
054800         MOVE RESP-IDKOLLI-NY    TO MOD-IDKOLLI-NY                        
054900       END-IF                                                             
055000     END-IF                                                               
055100                                                                          
055200     MOVE RESP-KDKOLLI-ATTR      TO MOD-KDKOLLI-ATTR                      
055300     IF RESP-KDKOLLI  = SPACE                                             
055400       MOVE MFS-RENSA-FAELT      TO MOD-KDKOLLI                           
055500     ELSE                                                                 
055600       IF RESP-KDKOLLI  = ALL '+'                                         
055700         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDKOLLI                           
055800       ELSE                                                               
055900         MOVE RESP-KDKOLLI       TO MOD-KDKOLLI                           
056000       END-IF                                                             
056100     END-IF                                                               
056200                                                                          
056300     MOVE RESP-KDEMBTYP-ATTR     TO MOD-KDEMBTYP-ATTR                     
056400     IF RESP-KDEMBTYP = SPACE                                             
056500       MOVE MFS-RENSA-FAELT      TO MOD-KDEMBTYP                          
056600     ELSE                                                                 
056700       IF RESP-KDEMBTYP = ALL '+'                                         
056800         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDEMBTYP                          
056900       ELSE                                                               
057000         MOVE RESP-KDEMBTYP      TO MOD-KDEMBTYP                          
057100       END-IF                                                             
057200     END-IF                                                               
057300                                                                          
057400     MOVE RESP-VKORDBTO-ATTR     TO MOD-VKORDBTO-ATTR                     
057500     IF RESP-VKORDBTO = SPACE                                             
057600       MOVE MFS-RENSA-FAELT      TO MOD-VKORDBTO                          
057700     ELSE                                                                 
057800       IF RESP-VKORDBTO = ALL '+'                                         
057900         MOVE MFS-ROER-EJ-FAELT  TO MOD-VKORDBTO                          
058000       ELSE                                                               
058100         MOVE RESP-VKORDBTO      TO MOD-VKORDBTO                          
058200       END-IF                                                             
058300     END-IF                                                               
058400                                                                          
058500     MOVE RESP-DIKOLLIL-ATTR     TO MOD-DIKOLLIL-ATTR                     
058600     IF RESP-DIKOLLIL = SPACE                                             
058700       MOVE MFS-RENSA-FAELT      TO MOD-DIKOLLIL                          
058800     ELSE                                                                 
058900       IF RESP-DIKOLLIL = ALL '+'                                         
059000         MOVE MFS-ROER-EJ-FAELT  TO MOD-DIKOLLIL                          
059100       ELSE                                                               
059200         MOVE RESP-DIKOLLIL      TO MOD-DIKOLLIL                          
059300       END-IF                                                             
059400     END-IF                                                               
059500                                                                          
059600     MOVE RESP-DIKOLLIB-ATTR     TO MOD-DIKOLLIB-ATTR                     
059700     IF RESP-DIKOLLIB = SPACE                                             
059800       MOVE MFS-RENSA-FAELT      TO MOD-DIKOLLIB                          
059900     ELSE                                                                 
060000       IF RESP-DIKOLLIB = ALL '+'                                         
060100         MOVE MFS-ROER-EJ-FAELT  TO MOD-DIKOLLIB                          
060200       ELSE                                                               
060300         MOVE RESP-DIKOLLIB      TO MOD-DIKOLLIB                          
060400       END-IF                                                             
060500     END-IF                                                               
060600                                                                          
060700     MOVE RESP-DIKOLLIH-ATTR     TO MOD-DIKOLLIH-ATTR                     
060800     IF RESP-DIKOLLIH = SPACE                                             
060900       MOVE MFS-RENSA-FAELT      TO MOD-DIKOLLIH                          
061000     ELSE                                                                 
061100       IF RESP-DIKOLLIH = ALL '+'                                         
061200         MOVE MFS-ROER-EJ-FAELT  TO MOD-DIKOLLIH                          
061300       ELSE                                                               
061400         MOVE RESP-DIKOLLIH      TO MOD-DIKOLLIH                          
061500       END-IF                                                             
061600     END-IF                                                               
061700                                                                          
061800                                                                          
061900     MOVE +1       TO INDX                                                
062000     PERFORM UNTIL INDX > RESP-KVRADER                                    
062100                                                                          
062200       IF RESP-FLNOLLAD-LINE (INDX) = SPACE                               
062300         MOVE MFS-RENSA-FAELT    TO MOD-FLNOLLAD (INDX)                   
062400       ELSE                                                               
062500         IF RESP-FLNOLLAD-LINE (INDX) = ALL '+'                           
062600           MOVE MFS-ROER-EJ-FAELT                                         
062700                                 TO MOD-FLNOLLAD (INDX)                   
062800         ELSE                                                             
062900           MOVE RESP-FLNOLLAD-LINE (INDX)                                 
063000                                 TO MOD-FLNOLLAD (INDX)                   
063100         END-IF                                                           
063200       END-IF                                                             
063300                                                                          
063400       IF RESP-IDPURAD-LINE (INDX) = SPACE                                
063500         MOVE MFS-RENSA-FAELT    TO MOD-IDPURAD (INDX)                    
063600       ELSE                                                               
063700         IF RESP-IDPURAD-LINE (INDX) = ALL '+'                            
063800           MOVE MFS-ROER-EJ-FAELT                                         
063900                                 TO MOD-IDPURAD (INDX)                    
064000         ELSE                                                             
064100           MOVE RESP-IDPURAD-LINE (INDX)                                  
064200                                 TO MOD-IDPURAD (INDX)                    
064300         END-IF                                                           
064400       END-IF                                                             
064500                                                                          
064600       IF RESP-IDARTNR-LINE (INDX) = SPACE                                
064700         MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR (INDX)                    
064800       ELSE                                                               
064900         IF RESP-IDARTNR-LINE (INDX) = ALL '+'                            
065000           MOVE MFS-ROER-EJ-FAELT                                         
065100                                 TO MOD-IDARTNR (INDX)                    
065200         ELSE                                                             
065300           MOVE RESP-IDARTNR-LINE (INDX)                                  
065400                                 TO MOD-IDARTNR (INDX)                    
065500         END-IF                                                           
065600       END-IF                                                             
065700                                                                          
065800       IF RESP-KVLEVART-LINE (INDX) = SPACE                               
065900         MOVE MFS-RENSA-FAELT    TO MOD-KVLEVART-UT (INDX)                
066000       ELSE                                                               
066100         IF RESP-KVLEVART-LINE (INDX) = ALL '+'                           
066200           MOVE MFS-ROER-EJ-FAELT                                         
066300                                 TO MOD-KVLEVART-UT (INDX)                
066400         ELSE                                                             
066500           MOVE RESP-KVLEVART-LINE (INDX)                                 
066600                                 TO MOD-KVLEVART-UT (INDX)                
066700         END-IF                                                           
066800       END-IF                                                             
066900                                                                          
067000       IF RESP-KDSVAR-LINE (INDX) = SPACE                                 
067100         MOVE MFS-RENSA-FAELT    TO MOD-KDSVAR-RAD (INDX)                 
067200       ELSE                                                               
067300         IF RESP-KDSVAR-LINE (INDX) = ALL '+'                             
067400           MOVE MFS-ROER-EJ-FAELT                                         
067500                                 TO MOD-KDSVAR-RAD (INDX)                 
067600         ELSE                                                             
067700           MOVE RESP-KDSVAR-LINE (INDX)                                   
067800                                 TO MOD-KDSVAR-RAD (INDX)                 
067900         END-IF                                                           
068000       END-IF                                                             
068100                                                                          
068200       MOVE RESP-IDKOLLI-LINE-ATTR (INDX)                                 
068300                                 TO MOD-IDKOLLI-RAD-ATTR (INDX)           
068400       IF RESP-IDKOLLI-LINE (INDX) = SPACE                                
068500         MOVE MFS-RENSA-FAELT    TO MOD-IDKOLLI-RAD (INDX)                
068600       ELSE                                                               
068700         IF RESP-IDKOLLI-LINE (INDX) = ALL '+'                            
068800           MOVE MFS-ROER-EJ-FAELT                                         
068900                                 TO MOD-IDKOLLI-RAD (INDX)                
069000         ELSE                                                             
069100           MOVE RESP-IDKOLLI-LINE (INDX)                                  
069200                                 TO MOD-IDKOLLI-RAD (INDX)                
069300         END-IF                                                           
069400       END-IF                                                             
069500                                                                          
069600       MOVE RESP-KVLEVART-LINE-IN-ATTR (INDX)                             
069700                                 TO MOD-KVLEVART-IN-ATTR (INDX)           
069800       IF RESP-KVLEVART-LINE-IN (INDX) = SPACE                            
069900         MOVE MFS-RENSA-FAELT    TO MOD-KVLEVART-IN (INDX)                
070000       ELSE                                                               
070100         IF RESP-KVLEVART-LINE-IN (INDX) = ALL '+'                        
070200           MOVE MFS-ROER-EJ-FAELT                                         
070300                                 TO MOD-KVLEVART-IN (INDX)                
070400         ELSE                                                             
070500           MOVE RESP-KVLEVART-LINE-IN (INDX)                              
070600                                 TO MOD-KVLEVART-IN (INDX)                
070700         END-IF                                                           
070800       END-IF                                                             
070900                                                                          
071000       ADD +1               TO INDX                                       
071100     END-PERFORM                                                          
071200                                                                          
071300*    PERFORM STARTS WITH PREVIOUS INDX VALUE                              
071400     PERFORM UNTIL INDX > MAX-KVRADER                                     
071500       MOVE MFS-RENSA-FAELT      TO MOD-FLNOLLAD (INDX)                   
071600                                    MOD-IDPURAD (INDX)                    
071700                                    MOD-IDARTNR (INDX)                    
071800                                    MOD-KVLEVART-UT (INDX)                
071900                                    MOD-KDSVAR-RAD (INDX)                 
072000                                    MOD-IDKOLLI-RAD (INDX)                
072100                                    MOD-KVLEVART-IN (INDX)                
072200       ADD  +1 TO INDX                                                    
072300     END-PERFORM                                                          
072400                                                                          
072500     .                                                                    
072600     EJECT                                                                
072700* IMS SEKTIONER                                                           
072800     SKIP3                                                                
072900                                                                          
073000 IMS-GET-MSG SECTION.                                                     
073100     MOVE '  QC' TO GODK-STATUSKODER                                      
073200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
073300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073400     PERFORM IMS-STATUSKONTROLL                                           
073500     SKIP3                                                                
073600                                                                          
073700     .                                                                    
073800 IMS-INSERT-MSG SECTION.                                                  
073900                                                                          
074000     IF WS-MSGI-IDLAND-SPR NOT = 'GB'                                     
074100         MOVE '0' TO MFS-KDHUVOMR                                         
074200     END-IF                                                               
074300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
074400     MOVE SPACE TO GODK-STATUSKODER                                       
074500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
074600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074700     PERFORM IMS-STATUSKONTROLL                                           
074800                                                                          
074900     .                                                                    
075000     EJECT                                                                
075100 IMS-STATUSKONTROLL SECTION.                                              
075200     SET STATUS-IX TO 1                                                   
075300     SEARCH GODK-STATUS AT END CALL FELLOG                                
075400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
075500     END-SEARCH                                                           
075600     .                                                                    
075700     EJECT                                                                
