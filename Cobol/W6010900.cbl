000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6010900.                                                
000400*AUTHOR.         LARS THELL > RAHUL JAIN.                                 
000500*DATE-WRITTEN.   92/04/15 > JUN 2012.                                     
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BILDEN VISAR INFORMATION OM PARTIET, MOTSVARANDE DEN             
001100*        UTSKRIVNA ARBETSRAPPORTEN/CLEARINGRAPPORTEN.                     
001200*        UTSKRIFT AV ARTBETSRAPPORT/CLEARINGRAPPORT KAN SKE               
001300*        FRÅN DENNA BILD.                                                 
001400*                                                                         
001500*        PROGRAMMET LÄSER      W6INLA (W6D1)                              
001600*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001700*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001800*        PROGRAMMET LÄSER      WLARTD (WDD8)                              
001900*        PROGRAMMET LÄSER      WDF5                                       
002000*        PROGRAMMET LÄSER      WDB6                                       
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W6T109                                              
002400*        MID:         W6I10901                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W6O10901                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6010900'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
004500 77  MAX-INDX                    PIC S9(4)  VALUE +6    COMP SYNC.        
004600                                                                          
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
004900 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
005000 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
005100                                                                          
005200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005300     88  NYCKLAR-OK                          VALUE 'J'.                   
005400     88  NYCKLAR-FEL                         VALUE 'N'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '6109'.                
005800     88  GODK-MID                            VALUE '6109'.                
005900     88  HELP-MID                            VALUE '0551'.                
006000     EJECT                                                                
006100 01  MESSAGE-CODES.                                                       
006200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  W611ADR                 PIC X(8)    VALUE 'W611ADR '.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  W6010910                PIC X(8)    VALUE 'W6010910'.            
007000     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
007100     EJECT                                                                
007200*01 -COPY WMSGINIT                                                        
007300     EJECT                                                                
007400*01 -COPY WWOMVAND                                                        
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007700*01 -COPY WMEDAREA                                                        
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL SUBPROGRAM W611ADR                               
008000*01 -COPY W611ADR                                                         
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
008300 01  REQU-AREA.                                                           
008400*    03 -COPY WZ01REQU                                                    
008500*    03 -COPY W60109I1                                                    
008600     EJECT                                                                
008700*                                                                         
008800 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
008900 01  RESP-AREA.                                                           
009000*    03 -COPY WZ01RESP                                                    
009100*    03 -COPY W60109O1                                                    
009200     EJECT                                                                
009300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009600     SKIP3                                                                
009700*01  MID -COPY W6I10901                                                   
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010000     SKIP3                                                                
010100*01  -COPY WMSGAREA                                                       
010200     EJECT                                                                
010300     03  MOD REDEFINES MSG-AREA.                                          
010400*      05  -COPY W6O10901                                                 
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010700     SKIP3                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
010900     SKIP3                                                                
011000*01  -COPY WL01MCNV                                                       
011100     EJECT                                                                
011200*01  -COPY WMFSAREA                                                       
011300     EJECT                                                                
011400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011500*                                                                         
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011800     SKIP3                                                                
011900*01  NYCKLAR-TILL-DLI.                                                    
012000                                                                          
012100     SKIP2                                                                
012200*    --- STATUS-KOD FRÅN IMS                                              
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FINNS                       VALUE '  '.                  
012500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012800     SKIP2                                                                
012900 01  GODK-STATUSKODER.                                                    
013000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     EJECT                                                                
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600                                                                          
013700*01  -COPY W0009   -PRE MSG-                                              
013800     EJECT                                                                
013900*01  -COPY W0008  -PRE USEA-                                              
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200*01  -COPY W0008  -PRE INLA-                                              
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500*01  -COPY W0008  -PRE INLC-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800*01  -COPY W0008  -PRE INLD-                                              
014900     05  FILLER                  PIC X.                                   
015000     EJECT                                                                
015100*01  -COPY W0008  -PRE PLAA-                                              
015200     05  FILLER                  PIC X.                                   
015300     EJECT                                                                
015400*01  -COPY W0008  -PRE ARTC-                                              
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015700*01  -COPY W0008  -PRE ARTD-                                              
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
016000*01  -COPY W0008  -PRE WDF5-                                              
016100     05  FILLER                  PIC X.                                   
016200     EJECT                                                                
016300*01  -COPY W0008  -PRE WDK7-                                              
016400     05  FILLER                  PIC X.                                   
016500     EJECT                                                                
016600*01  -COPY W0008  -PRE WDB6-                                              
016700     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
016810*01  -COPY W0008  -PRE BENA-                                              
016820     05  FILLER                  PIC X.                                   
016830     EJECT                                                                
016900***  PCB'ER TILL SUBPGM W611ADR                                           
017000                                                                          
017100 01  ADR-INLA-PCB                PIC X.                                   
017200                                                                          
017300 01  ADR-INLC-PCB                PIC X.                                   
017400                                                                          
017500 01  ADR-PLAA-PCB                PIC X.                                   
017600                                                                          
017700 01  ADR-WDK6-PCB                PIC X.                                   
017800                                                                          
017900 01  STYR-HANA-PCB               PIC X.                                   
018000                                                                          
018100 01  STYR-PLAA-PCB               PIC X.                                   
018200                                                                          
018300     EJECT                                                                
018400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB INLA-PCB INLC-PCB             
018500                 INLD-PCB                                                 
018600                 PLAA-PCB ARTC-PCB ARTD-PCB WDF5-PCB WDK7-PCB             
018700                 WDB6-PCB BENA-PCB                                        
018800          ADR-INLA-PCB ADR-INLC-PCB ADR-PLAA-PCB ADR-WDK6-PCB             
018900                 STYR-HANA-PCB STYR-PLAA-PCB.                             
019000                                                                          
019100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB INLA-PCB INLC-PCB             
019200                 INLD-PCB                                                 
019300                 PLAA-PCB ARTC-PCB ARTD-PCB WDF5-PCB WDK7-PCB             
019400                 WDB6-PCB BENA-PCB                                        
019500          ADR-INLA-PCB ADR-INLC-PCB ADR-PLAA-PCB ADR-WDK6-PCB             
019600                 STYR-HANA-PCB STYR-PLAA-PCB.                             
019700                                                                          
019800     PERFORM IMS-GET-MSG                                                  
019900     IF SEGMENT-FINNS                                                     
020000       PERFORM A-INIT                                                     
020100       PERFORM B-INIT-KEYS                                                
020200       IF NYCKLAR-OK                                                      
020300           PERFORM F-CALL-BIZ-LOGIC-W6010910                              
020400       END-IF                                                             
020500       IF MFS-PRINT                                                       
020600         IF RESP-ADINLOMR-PRT NOT = ALL '+'                               
020700           MOVE RESP-ADINLOMR-PRT TO MOD-ADINLOMR-PRT                     
020800           PERFORM MFS-LAES-IN-IGEN                                       
020900         ELSE                                                             
021000           MOVE MFS-RENSA-FAELT   TO MOD-ADINLOMR-PRT                     
021100         END-IF                                                           
021200       END-IF                                                             
021300                                                                          
021400       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O10901 + 4                      
021500       PERFORM IMS-INSERT-MSG                                             
021600     END-IF                                                               
021700                                                                          
021800     MOVE ZERO        TO RETURN-CODE                                      
021900     GOBACK                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 A-INIT SECTION.                                                          
022300                                                                          
022400     IF MSG-DUBBLA-TRANSKODER                                             
022500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I10901                 
022600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
022700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022800     ELSE                                                                 
022900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I10901                  
023000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023200     END-IF                                                               
023300                                                                          
023400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
023500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
023600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
023700                                                                          
023800     MOVE LOW-VALUE   TO MSG-AREA                                         
023900     MOVE 'W6O109N1'  TO MFS-IDMOD                                        
024000     MOVE '6109'      TO MOD-IDTRANS                                      
024100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
024200                                                                          
024300     IF NOT EGEN-MID AND NOT HELP-MID                                     
024400       MOVE SPACE     TO MFS-KDTRTYP                                      
024500       MOVE '7'       TO MFS-IDPFK                                        
024600     END-IF                                                               
024700                                                                          
024800     PERFORM AA-INIT-NYCKLAR                                              
024900                                                                          
025000     IF MSGI-IDLAND-SPR = 'GB'                                            
025100       MOVE 'GB'      TO MCNV-IDSPRAK                                     
025200     ELSE                                                                 
025300       MOVE 'SV'      TO MCNV-IDSPRAK                                     
025400     END-IF                                                               
025500                                                                          
025600     MOVE '101'                     TO REQU-IDMSGVER                      
025700     MOVE MSGI-IDUSER               TO REQU-IDUSER                        
025800     MOVE MSGI-IDSPRAK              TO REQU-IDSPRAK                       
025900     MOVE MSGI-KDMATT               TO REQU-KDMATT                        
026000     .                                                                    
026100     EJECT                                                                
026200*----------------------------------------------------------------*        
026300 AA-INIT-NYCKLAR SECTION.                                                 
026400                                                                          
026500     MOVE ALL '+' TO MSGI-WMSGINIT                                        
026600     MOVE '001'                  TO MSGI-KDCALL                           
026700     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
026800     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
026900     MOVE '6109'                 TO MSGI-IDTRANS                          
027000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027100     .                                                                    
027200     EJECT                                                                
027300 B-INIT-KEYS     SECTION.                                                 
027400                                                                          
027500     MOVE JA TO NYCKLAR-SW                                                
027600                                                                          
027700     PERFORM BA-KOLLA-IDLEVNR-KOLLI                                       
027800     PERFORM BB-KOLLA-IDOKOLLI                                            
027900     PERFORM BC-KOLLA-IDLOPNRM                                            
028000     PERFORM BD-KOLLA-IDDC                                                
028100                                                                          
028200     MOVE MID-ADINLOMR-PRT     TO REQU-ADINLOMR-PRT                       
028300                                                                          
028400     IF GODK-MID OR NYCKLAR-OK                                            
028500         MOVE WS-IDLEVNR-KOLLI TO MOD-IDLEVNR-KOLLI-UT                    
028600                                  REQU-IDLEVNR-KOLLI-KEY                  
028700         MOVE WS-IDOKOLLI      TO MOD-IDOKOLLI-UT                         
028800                                  REQU-IDOKOLLI-KEY                       
028900         INSPECT MOD-IDOKOLLI-UT REPLACING LEADING ZERO BY SPACE          
029000         MOVE WS-IDLOPNRM      TO MOD-IDLOPNRM-UT                         
029100                                  REQU-IDLOPNRM-KEY                       
029200         INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE          
029300         MOVE REQU-IDDC-KEY    TO MOD-IDDC-UT                             
029400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PRT-ATTR              
029500                                                                          
029600         IF MFS-PRINT                                                     
029700            SET REQU-PRINT     TO TRUE                                    
029800         END-IF                                                           
029900     ELSE                                                                 
030000         MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-KOLLI-UT                    
030100                                  MOD-IDOKOLLI-UT                         
030200                                  MOD-IDLOPNRM-UT                         
030300                                  MOD-IDDC-UT                             
030400                                  MOD-ADINLOMR-PRT                        
030500         MOVE MFS-FORMATETS-ATTR TO MOD-ADINLOMR-PRT-ATTR                 
030600     END-IF                                                               
030700                                                                          
030800     IF NYCKLAR-FEL                                                       
030801       IF GODK-MID                                                        
030810         MOVE ERR-WRONG-KEY    TO MCNV-IDMSG-ERROR                        
030820         MOVE SPACE            TO MCNV-IDMSG-INFO                         
030840                                                                          
030850         CALL WL01MCNV USING MCNV-AREA                                    
030860                                                                          
030870         MOVE MCNV-MFSINF      TO MOD-TEMFSINF                            
030880         MOVE MCNV-MFSFEL      TO MOD-TEMFSFEL                            
031100       END-IF                                                             
031200       PERFORM MFS-RENSA-FAELT-UT                                         
031300     END-IF                                                               
031400     .                                                                    
031500     EJECT                                                                
031600 BA-KOLLA-IDLEVNR-KOLLI SECTION.                                          
031700                                                                          
031800     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI-IN                         
031900                                                                          
032000     IF MID-IDLEVNR-KOLLI-IN        = ALL '+'                             
032100         MOVE MID-IDLEVNR-KOLLI-UT  TO WS-IDLEVNR-KOLLI                   
032200      ELSE                                                                
032300         MOVE MID-IDLEVNR-KOLLI-IN  TO WS-IDLEVNR-KOLLI                   
032400         MOVE '7'                   TO MFS-IDPFK                          
032500         MOVE SPACE                 TO MFS-KDTRTYP                        
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900 BB-KOLLA-IDOKOLLI SECTION.                                               
033000                                                                          
033100     MOVE MFS-RENSA-FAELT      TO MOD-IDOKOLLI-IN                         
033200                                                                          
033300     IF MID-IDOKOLLI-IN        = ALL '+'                                  
033400         MOVE MID-IDOKOLLI-UT  TO WS-IDOKOLLI                             
033500         INSPECT WS-IDOKOLLI REPLACING LEADING SPACE BY ZERO              
033600      ELSE                                                                
033700         MOVE MID-IDOKOLLI-IN  TO WS-IDOKOLLI                             
033800         MOVE '7'              TO MFS-IDPFK                               
033900         MOVE SPACE            TO MFS-KDTRTYP                             
034000     END-IF                                                               
034100                                                                          
034200     IF WS-IDOKOLLI NUMERIC                                               
034300         CONTINUE                                                         
034400     ELSE                                                                 
034500         MOVE NEJ              TO NYCKLAR-SW                              
034600     END-IF                                                               
034700     .                                                                    
034800     EJECT                                                                
034900 BC-KOLLA-IDLOPNRM SECTION.                                               
035000                                                                          
035100     MOVE MFS-RENSA-FAELT      TO MOD-IDLOPNRM-IN                         
035200                                                                          
035300     IF MID-IDLOPNRM-IN        = ALL '+'                                  
035400         MOVE MID-IDLOPNRM-UT  TO WS-IDLOPNRM                             
035500         INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO              
035600      ELSE                                                                
035700         MOVE MID-IDLOPNRM-IN  TO WS-IDLOPNRM                             
035800         MOVE '7'              TO MFS-IDPFK                               
035900         MOVE SPACE            TO MFS-KDTRTYP                             
036000     END-IF                                                               
036100                                                                          
036200     IF WS-IDLOPNRM NUMERIC                                               
036300         CONTINUE                                                         
036400     ELSE                                                                 
036500         MOVE NEJ              TO NYCKLAR-SW                              
036600     END-IF                                                               
036700     .                                                                    
036800     EJECT                                                                
036900 BD-KOLLA-IDDC     SECTION.                                               
037000                                                                          
037100     MOVE MFS-RENSA-FAELT      TO MOD-IDDC-IN                             
037200                                                                          
037300     IF MID-IDDC-IN        = ALL '+'                                      
037400         MOVE MSGI-IDDC        TO REQU-IDDC-KEY                           
037500      ELSE                                                                
037600         MOVE MID-IDDC-IN      TO REQU-IDDC-KEY                           
037700         MOVE '7'              TO MFS-IDPFK                               
037800         MOVE SPACE            TO MFS-KDTRTYP                             
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 F-CALL-BIZ-LOGIC-W6010910 SECTION.                                       
038300                                                                          
038400     CALL W6010910 USING REQU-AREA RESP-AREA                              
038500                         INLA-PCB INLC-PCB INLD-PCB                       
038600                         PLAA-PCB ARTC-PCB ARTD-PCB WDF5-PCB              
038700                         WDK7-PCB WDB6-PCB BENA-PCB                       
038800                         ADR-INLA-PCB ADR-INLC-PCB ADR-PLAA-PCB           
038900                         ADR-WDK6-PCB STYR-HANA-PCB STYR-PLAA-PCB.        
039000                                                                          
039100     PERFORM FA-SET-MSG-AND-HILIGHT                                       
039200     PERFORM FB-MOVE-RESP-TO-MOD                                          
039300                                                                          
039400     .                                                                    
039500     EJECT                                                                
039600 FA-SET-MSG-AND-HILIGHT SECTION.                                          
039700                                                                          
039800     MOVE RESP-IDMSG-ERROR          TO MCNV-IDMSG-ERROR                   
039900     MOVE RESP-IDMSG-INFO           TO MCNV-IDMSG-INFO                    
040000     MOVE RESP-IDELMT-ERROR         TO MCNV-IDELMT-ERROR                  
040100                                                                          
040200     CALL WL01MCNV USING MCNV-AREA                                        
040300                                                                          
040400     MOVE MCNV-MFSINF               TO MOD-TEMFSINF                       
040500     MOVE MCNV-MFSFEL               TO MOD-TEMFSFEL                       
040600     .                                                                    
040700     EJECT                                                                
040800 FB-MOVE-RESP-TO-MOD SECTION.                                             
040900                                                                          
041000     IF (REQU-IDLEVNR-KOLLI-KEY  NOT = SPACE AND                          
041100        (REQU-IDOKOLLI-KEY       NUMERIC AND                              
041200         REQU-IDOKOLLI-KEY       >  ZERO))                                
041300                    OR                                                    
041400        (REQU-IDLOPNRM-KEY       = ALL '+' AND                            
041500         WS-IDLEVNR-KOLLI       NOT = SPACE AND                           
041600         WS-IDOKOLLI            > ZERO)                                   
041700         CONTINUE                                                         
041800     ELSE                                                                 
041900         MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-KOLLI-UT               
042000                                       MOD-IDOKOLLI-UT                    
042100     END-IF                                                               
042200                                                                          
042300     IF RESP-ADINLOMR-PRT = SPACE OR ALL '+'                              
042400         MOVE MFS-RENSA-FAELT       TO MOD-ADINLOMR-PRT                   
042500     ELSE                                                                 
042600         MOVE RESP-ADINLOMR-PRT     TO MOD-ADINLOMR-PRT                   
042700     END-IF                                                               
042800                                                                          
042900     IF RESP-IDLBBET = SPACE                                              
043000        MOVE MFS-RENSA-FAELT        TO MOD-IDLBBET                        
043100     ELSE                                                                 
043200        MOVE RESP-IDLBBET           TO MOD-IDLBBET                        
043300     END-IF                                                               
043400                                                                          
043500     IF RESP-FLAR = SPACE                                                 
043600        MOVE MFS-RENSA-FAELT        TO MOD-FLAR                           
043700     ELSE                                                                 
043800        MOVE RESP-FLAR              TO MOD-FLAR                           
043900     END-IF                                                               
044000                                                                          
044100     IF RESP-IDFS = SPACE                                                 
044200        MOVE MFS-RENSA-FAELT        TO MOD-IDFS                           
044300     ELSE                                                                 
044400        MOVE RESP-IDFS              TO MOD-IDFS                           
044500     END-IF                                                               
044600                                                                          
044700     IF RESP-IDLEVNR = SPACE                                              
044800        MOVE MFS-RENSA-FAELT        TO MOD-IDLEVNR                        
044900     ELSE                                                                 
045000        MOVE RESP-IDLEVNR           TO MOD-IDLEVNR                        
045100     END-IF                                                               
045200                                                                          
045300     IF RESP-TIAVIDAT = SPACE                                             
045400        MOVE MFS-RENSA-FAELT        TO MOD-TIAVIDAT                       
045500     ELSE                                                                 
045600        MOVE RESP-TIAVIDAT          TO MOD-TIAVIDAT                       
045700     END-IF                                                               
045800                                                                          
045900     IF RESP-TIINLMOT = SPACE                                             
046000        MOVE MFS-RENSA-FAELT        TO MOD-TIINLMOT                       
046100     ELSE                                                                 
046200        MOVE RESP-TIINLMOT          TO MOD-TIINLMOT                       
046300     END-IF                                                               
046400                                                                          
046500     IF RESP-ADLAGOMR = SPACE                                             
046600        MOVE MFS-RENSA-FAELT        TO MOD-ADLAGOMR                       
046700     ELSE                                                                 
046800        MOVE RESP-ADLAGOMR          TO MOD-ADLAGOMR                       
046900     END-IF                                                               
047000                                                                          
047100     IF RESP-ADGANG = SPACE                                               
047200        MOVE MFS-RENSA-FAELT        TO MOD-ADGANG                         
047300     ELSE                                                                 
047400        MOVE RESP-ADGANG            TO MOD-ADGANG                         
047500     END-IF                                                               
047600                                                                          
047700     IF RESP-ADPLATS = SPACE                                              
047800        MOVE MFS-RENSA-FAELT        TO MOD-ADPLATS                        
047900     ELSE                                                                 
048000        MOVE RESP-ADPLATS           TO MOD-ADPLATS                        
048100     END-IF                                                               
048200                                                                          
048300     IF RESP-BEFT = SPACE                                                 
048400        MOVE MFS-RENSA-FAELT        TO MOD-BEFT                           
048500     ELSE                                                                 
048600        MOVE RESP-BEFT              TO MOD-BEFT                           
048700     END-IF                                                               
048800                                                                          
048900     IF RESP-IDARTNR = SPACE                                              
049000        MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR                        
049100     ELSE                                                                 
049200        MOVE RESP-IDARTNR           TO MOD-IDARTNR                        
049300     END-IF                                                               
049400                                                                          
049500     IF RESP-KDLAGEMB = SPACE                                             
049600        MOVE MFS-RENSA-FAELT        TO MOD-KDLAGEMB                       
049700     ELSE                                                                 
049800        MOVE RESP-KDLAGEMB          TO MOD-KDLAGEMB                       
049900     END-IF                                                               
050000                                                                          
050100     IF RESP-KDRT IS NUMERIC                                              
050200        MOVE RESP-KDRT              TO MOD-KDRT                           
050300     ELSE                                                                 
050400        MOVE MFS-RENSA-FAELT        TO MOD-KDRT                           
050500     END-IF                                                               
050600                                                                          
050700     IF RESP-KDSORT = SPACE                                               
050800        MOVE MFS-RENSA-FAELT        TO MOD-KDSORT                         
050900     ELSE                                                                 
051000        MOVE RESP-KDSORT            TO MOD-KDSORT                         
051100     END-IF                                                               
051200                                                                          
051300     IF RESP-FLKVAANT = SPACE OR ALL '+'                                  
051400        MOVE MFS-RENSA-FAELT        TO MOD-FLKVAANT                       
051500     ELSE                                                                 
051600        MOVE RESP-FLKVAANT          TO MOD-FLKVAANT                       
051700     END-IF                                                               
051800                                                                          
051900     IF RESP-KVAVIS-HUV = SPACE                                           
052000        MOVE MFS-RENSA-FAELT        TO MOD-KVAVIS-HUV                     
052100     ELSE                                                                 
052200        MOVE RESP-KVAVIS-HUV        TO MOD-KVAVIS-HUV                     
052300     END-IF                                                               
052400                                                                          
052500     IF RESP-ADTRDEST = SPACE                                             
052600        MOVE MFS-RENSA-FAELT        TO MOD-ADTRDEST                       
052700     ELSE                                                                 
052800        MOVE RESP-ADTRDEST          TO MOD-ADTRDEST                       
052900     END-IF                                                               
053000                                                                          
053100     IF RESP-KVAVIS-KIT = SPACE                                           
053200        MOVE MFS-RENSA-FAELT        TO MOD-KVAVIS-KIT                     
053300     ELSE                                                                 
053400        MOVE RESP-KVAVIS-KIT        TO MOD-KVAVIS-KIT                     
053500     END-IF                                                               
053600                                                                          
053700     IF RESP-VKART = SPACE                                                
053800        MOVE MFS-RENSA-FAELT        TO MOD-VKART                          
053900     ELSE                                                                 
054000        MOVE RESP-VKART             TO MOD-VKART                          
054100     END-IF                                                               
054200                                                                          
054300     IF RESP-KDSORT-VIKT = SPACE OR ALL '+'                               
054400        MOVE MFS-RENSA-FAELT        TO MOD-KDSORT-VIKT                    
054500     ELSE                                                                 
054600        MOVE RESP-KDSORT-VIKT       TO MOD-KDSORT-VIKT                    
054700     END-IF                                                               
054800                                                                          
054900     IF RESP-VLARTNTO = SPACE                                             
055000        MOVE MFS-RENSA-FAELT        TO MOD-VLARTNTO                       
055100     ELSE                                                                 
055200        MOVE RESP-VLARTNTO          TO MOD-VLARTNTO                       
055300     END-IF                                                               
055400                                                                          
055500     IF RESP-BESORT-VOLYM = SPACE OR ALL '+'                              
055600        MOVE MFS-RENSA-FAELT        TO MOD-BESORT-VOLYM                   
055700     ELSE                                                                 
055800        MOVE RESP-BESORT-VOLYM      TO MOD-BESORT-VOLYM                   
055900     END-IF                                                               
056000                                                                          
056100     IF RESP-KVAVIS = SPACE                                               
056200        MOVE MFS-RENSA-FAELT        TO MOD-KVAVIS                         
056300     ELSE                                                                 
056400        MOVE RESP-KVAVIS            TO MOD-KVAVIS                         
056500     END-IF                                                               
056600                                                                          
056700     IF RESP-KVAVIS-PRIO = SPACE                                          
056800        MOVE MFS-RENSA-FAELT        TO MOD-KVAVIS-PRIO                    
056900     ELSE                                                                 
057000        MOVE RESP-KVAVIS-PRIO       TO MOD-KVAVIS-PRIO                    
057100     END-IF                                                               
057200                                                                          
057300     IF RESP-KVKVAPRIM = SPACE                                            
057400        MOVE MFS-RENSA-FAELT        TO MOD-KVKVAPRIM                      
057500     ELSE                                                                 
057600        MOVE RESP-KVKVAPRIM         TO MOD-KVKVAPRIM                      
057700     END-IF                                                               
057800                                                                          
057900     IF RESP-KVKVASEK = SPACE                                             
058000        MOVE MFS-RENSA-FAELT        TO MOD-KVKVASEK                       
058100     ELSE                                                                 
058200        MOVE RESP-KVKVASEK          TO MOD-KVKVASEK                       
058300     END-IF                                                               
058400                                                                          
058500     IF RESP-IDLOPNRM-KEY = SPACE OR ALL '+'                              
058510        IF MID-IDLOPNRM-IN = ALL '+'                                      
058520           MOVE MFS-RENSA-FAELT     TO MOD-IDLOPNRM-UT                    
058530        ELSE                                                              
058600           MOVE MID-IDLOPNRM-IN     TO MOD-IDLOPNRM-UT                    
058610           INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE        
058620        END-IF                                                            
058700     ELSE                                                                 
058800        MOVE RESP-IDLOPNRM-KEY      TO MOD-IDLOPNRM-UT                    
058900     END-IF                                                               
059000                                                                          
059100     IF RESP-KDFARLIG-TEXT = SPACE OR ALL '+'                             
059200        MOVE MFS-RENSA-FAELT        TO MOD-KDFARLIG-TEXT                  
059300     ELSE                                                                 
059400        MOVE RESP-KDFARLIG-TEXT     TO MOD-KDFARLIG-TEXT                  
059500     END-IF                                                               
059600                                                                          
059700     MOVE +1                        TO INDX                               
059800     PERFORM UNTIL INDX        >  MAX-INDX                                
059900         IF RESP-ADINLOMR (INDX) = SPACE                                  
060000             MOVE MFS-RENSA-FAELT   TO MOD-ADINLOMR (INDX)                
060100         ELSE                                                             
060200             MOVE RESP-ADINLOMR (INDX) TO MOD-ADINLOMR (INDX)             
060300         END-IF                                                           
060400         ADD +1                     TO INDX                               
060500     END-PERFORM                                                          
060600                                                                          
060700     IF RESP-IDANSK = SPACE OR ALL '+'                                    
060800        MOVE MFS-RENSA-FAELT        TO MOD-IDANSK                         
060900     ELSE                                                                 
061000        MOVE RESP-IDANSK            TO MOD-IDANSK                         
061100     END-IF                                                               
061200                                                                          
061300     IF RESP-KDKONTR = SPACE                                              
061400        MOVE MFS-RENSA-FAELT        TO MOD-KDKONTR                        
061500     ELSE                                                                 
061600        MOVE RESP-KDKONTR           TO MOD-KDKONTR                        
061700     END-IF                                                               
061800                                                                          
061900     IF RESP-KVQPACK-3 = SPACE                                            
062000        MOVE MFS-RENSA-FAELT        TO MOD-KVQPACK-3                      
062100     ELSE                                                                 
062200        MOVE RESP-KVQPACK-3         TO MOD-KVQPACK-3                      
062300     END-IF                                                               
062400                                                                          
062500     IF RESP-KDARTURS = SPACE                                             
062600        MOVE MFS-RENSA-FAELT        TO MOD-KDARTURS                       
062700     ELSE                                                                 
062800        MOVE RESP-KDARTURS          TO MOD-KDARTURS                       
062900     END-IF                                                               
063000                                                                          
063100     IF RESP-BELEV1 = SPACE                                               
063200        MOVE MFS-RENSA-FAELT        TO MOD-BELEV1                         
063300     ELSE                                                                 
063400        MOVE RESP-BELEV1            TO MOD-BELEV1                         
063500     END-IF                                                               
063600                                                                          
063700     IF RESP-BELEV2 = SPACE                                               
063800        MOVE MFS-RENSA-FAELT        TO MOD-BELEV2                         
063900     ELSE                                                                 
064000        MOVE RESP-BELEV2            TO MOD-BELEV2                         
064100     END-IF                                                               
064200                                                                          
064300     IF RESP-ADBUFFOMR = SPACE                                            
064400        MOVE MFS-RENSA-FAELT        TO MOD-ADBUFFOMR                      
064500     ELSE                                                                 
064600        MOVE RESP-ADBUFFOMR         TO MOD-ADBUFFOMR                      
064700     END-IF                                                               
064800                                                                          
064900     IF RESP-ADBUFFGANG = SPACE                                           
065000        MOVE MFS-RENSA-FAELT        TO MOD-ADBUFFGANG                     
065100     ELSE                                                                 
065200        MOVE RESP-ADBUFFGANG        TO MOD-ADBUFFGANG                     
065300     END-IF                                                               
065400                                                                          
065500     IF RESP-ADBUFFPL = SPACE                                             
065600        MOVE MFS-RENSA-FAELT        TO MOD-ADBUFFPL                       
065700     ELSE                                                                 
065800        MOVE RESP-ADBUFFPL          TO MOD-ADBUFFPL                       
065900     END-IF                                                               
066000                                                                          
066010     IF RESP-BEART = SPACE                                                
066020        MOVE MFS-RENSA-FAELT        TO MOD-BEART                          
066030     ELSE                                                                 
066040        MOVE RESP-BEART             TO MOD-BEART                          
066050     END-IF                                                               
066051                                                                          
066060     IF RESP-KDKVAINL = SPACE                                             
066070        MOVE MFS-RENSA-FAELT        TO MOD-KDKVAINL                       
066080     ELSE                                                                 
066090        MOVE RESP-KDKVAINL          TO MOD-KDKVAINL                       
066091     END-IF                                                               
066100     .                                                                    
066200     EJECT                                                                
066300 MFS-RENSA-FAELT-UT SECTION.                                              
066400                                                                          
066500     MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR                             
066600                                  MOD-KVAVIS-HUV                          
066700                                  MOD-BEART                               
066710                                  MOD-KDKVAINL                            
066800                                  MOD-IDFS                                
066900                                  MOD-TIAVIDAT                            
067000                                  MOD-IDLBBET                             
067100                                  MOD-IDLEVNR                             
067200                                  MOD-TIINLMOT                            
067300                                  MOD-KVAVIS                              
067400                                  MOD-VKART                               
067500                                  MOD-ADLAGOMR                            
067600                                  MOD-ADGANG                              
067700                                  MOD-ADPLATS                             
067800                                  MOD-VLARTNTO                            
067900                                  MOD-KVAVIS-PRIO                         
068000                                  MOD-KDLAGEMB                            
068100                                  MOD-ADBUFFOMR                           
068200                                  MOD-ADBUFFGANG                          
068300                                  MOD-ADBUFFPL                            
068400                                  MOD-KDARTURS                            
068500                                  MOD-KVKVAPRIM                           
068600                                  MOD-BEFT                                
068700                                  MOD-IDANSK                              
068800                                  MOD-KVKVASEK                            
068900                                  MOD-KDSORT                              
069000                                  MOD-KDFARLIG-TEXT                       
069100                                  MOD-ADTRDEST                            
069200                                  MOD-KVAVIS-KIT                          
069300                                  MOD-KVQPACK-3                           
069400                                  MOD-KDKONTR                             
069500                                  MOD-FLAR                                
069600                                  MOD-BELEV1                              
069700                                  MOD-BELEV2                              
069800     MOVE +1                   TO INDX                                    
069900     PERFORM UNTIL INDX        >  MAX-INDX                                
070000         MOVE MFS-RENSA-FAELT  TO MOD-ADINLOMR (INDX)                     
070100         ADD +1                TO INDX                                    
070200     END-PERFORM                                                          
070300     .                                                                    
070400     EJECT                                                                
070500 MFS-LAES-IN-IGEN SECTION.                                                
070600                                                                          
070700     MOVE RESP-ADINLOMR-PRT-ATTR TO MOD-ADINLOMR-PRT-ATTR                 
070800     .                                                                    
070900     EJECT                                                                
071000* --- IMS SEKTIONER ---                                                   
071100     SKIP3                                                                
071200 IMS-GET-MSG SECTION.                                                     
071300                                                                          
071400     MOVE '  QC' TO GODK-STATUSKODER                                      
071500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
071600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071700     PERFORM IMS-STATUSKONTROLL                                           
071800     .                                                                    
071900     SKIP3                                                                
072000 IMS-INSERT-MSG SECTION.                                                  
072100                                                                          
072200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
072300       MOVE '0' TO MFS-KDHUVOMR                                           
072400     END-IF                                                               
072500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
072600     MOVE SPACE TO GODK-STATUSKODER                                       
072700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
072800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072900     PERFORM IMS-STATUSKONTROLL                                           
073000     .                                                                    
073100     EJECT                                                                
073200 IMS-STATUSKONTROLL SECTION.                                              
073300                                                                          
073400     SET STATUS-IX TO 1                                                   
073500     SEARCH GODK-STATUS                                                   
073600       AT END                                                             
073700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
073800         DELIMITED BY SIZE INTO FELTEXT                                   
073900         CALL FELLOG                                                      
074000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
074100         CONTINUE                                                         
074200     END-SEARCH                                                           
074300     .                                                                    
