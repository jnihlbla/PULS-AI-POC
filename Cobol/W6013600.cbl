000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6013600.                                                
000400*AUTHOR.         ANNELIE ENGLUND / ARCHANA BHAT.                          
000500*DATE-WRITTEN.   92/08/07 / JUNE 2012.                                    
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR EN MPP SOM ANVÄNDS NÄR MAN VILL FÖRPACKA           
001100*        ETT PARTI, ENSKILT KOLLI ELLER VID OMPACKNING.                   
001200*        FÖRBEHANDLINGSRAPPORT KAN BESTÄLLAS FRÅN BILDEN.                 
001300*        NÄR FÖRPACKNING/OMPACKNING ÄR UTFÖRD, BESTÄLLS FLAGGOR           
001400*        OCH ETIKETTER UT.                                                
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001700*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001800*        PROGRAMMET LÄSER      WDK6                                       
001900*        PROGRAMMET LÄSER      WDB6                                       
002000*        PROGRAMMET UPPDATERAR W6LOPA (W6G1)                              
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W6T136                                              
002400*        MID:         W6I13601                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W6O13601                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6013600'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
009000*    --- INDEX                                                            
009100 77  INDX                        PIC  S9(2)  VALUE ZERO.                  
009200 77  MAX-INDX                    PIC  S9(2)  VALUE +6.                    
010500                                                                          
011300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
011400 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
011500 77  WS-IDINLVGN                 PIC X(3)    VALUE SPACE.                 
011600 77  WS-ADINLOMR                 PIC X(4)    VALUE SPACE.                 
011700 77  WS-ADINLOMR-NXT             PIC X(4)    VALUE SPACE.                 
011800 77  WS-KDINLQ                   PIC X(1)    VALUE SPACE.                 
011900 77  WS-BEFT-FOM                 PIC X(2)    VALUE SPACE.                 
012000 77  WS-BEFT-TOM                 PIC X(2)    VALUE SPACE.                 
012100 77  WS-FLINLFB                  PIC X(1)    VALUE SPACE.                 
012200 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
012300 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
012400*                                                                         
013500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013600     88  EGEN-MID                            VALUE '6136'.                
013700     88  GODK-MID                            VALUE '6131' '6132'          
013800                                                   '6133' '6134'          
013900                                                   '6135' '6136'          
014000                                                   '6137' '6138'          
014100                                                   '6139'.                
014200     88  HELP-MID                            VALUE '0551'.                
014300                                                                          
021000     EJECT                                                                
021100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021200 01  GENERELLA-SUBPROGRAM.                                                
021400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
022000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
022010     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
022020     03  W6013610                PIC X(8)    VALUE 'W6013610'.            
022100     EJECT                                                                
022200*01 -COPY WMSGINIT                                                        
022300     SKIP3                                                                
022400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
022500*01 -COPY WMEDAREA                                                        
022600     SKIP3                                                                
024900 01  W-IDMSG-ERROR               PIC X(3).                                
025000     88   WRONG-KEY                          VALUE '022'.                 
025500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
025600*                                                                         
025700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
025800     SKIP3                                                                
025900*01  MID -COPY W6I13601                                                   
026000     EJECT                                                                
026100 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
026200     SKIP3                                                                
026300*01  -COPY WMSGAREA                                                       
026400     EJECT                                                                
026500     03  MOD REDEFINES MSG-AREA.                                          
026600*      05  -COPY W6O13601                                                 
026700     EJECT                                                                
026800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
026900     SKIP3                                                                
027000*01  -COPY WMFSAREA                                                       
027100     EJECT                                                                
027101*                                                                         
027102 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
027110 01  REQU-AREA.                                                           
027120*    03 -COPY WZ01REQU                                                    
027130*    03 -COPY W60136I1                                                    
027140 77  MAX-KVRADER                 PIC S9(4)  COMP VALUE +6.                
027150     EJECT                                                                
027160 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
027170 01  RESP-AREA.                                                           
027180*    03 -COPY WZ01RESP                                                    
027190*    03 -COPY W60136O1                                                    
027191*                                                                         
027192 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
027193     SKIP3                                                                
027194*01  -COPY WL01MCNV                                                       
034200     SKIP2                                                                
034300*    --- STATUS-KOD FRÅN IMS                                              
034400 01  STATUS-WS                   PIC XX.                                  
034500     88  SEGMENT-FINNS                       VALUE '  '.                  
034600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
034700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
034800     SKIP2                                                                
034900 01  GODK-STATUSKODER.                                                    
035000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035100     SKIP3                                                                
035200 01  SSA1                        PIC X(128).                              
035300 01  SSA2                        PIC X(64).                               
035400     EJECT                                                                
035500*    --- IMS FUNKTIONSKODER                                               
035600*01  -COPY W0003                                                          
035700     EJECT                                                                
045300     EJECT                                                                
048500 LINKAGE SECTION.                                                         
048600                                                                          
048700*01  -COPY W0009   -PRE MSG-                                              
048800     EJECT                                                                
048900*01  -COPY W0009   -PRE 6191-                                             
049000     EJECT                                                                
049100*01  -COPY W0009   -PRE 6194-                                             
049200     EJECT                                                                
049300*01  -COPY W0009   -PRE 6195-                                             
049400     EJECT                                                                
049500*01  -COPY W0009   -PRE 6197-                                             
049600     EJECT                                                                
049700*01  -COPY W0009   -PRE 6202-                                             
049800     EJECT                                                                
049900*01  -COPY W0009   -PRE DISP-                                             
050000     EJECT                                                                
050100*01  -COPY W0008  -PRE USEA-                                              
050200     05  FILLER                  PIC X.                                   
050300     EJECT                                                                
050400*01  -COPY W0008  -PRE INLA-                                              
050500     05  FILLER                  PIC X.                                   
050600     EJECT                                                                
050700*01  -COPY W0008  -PRE INLABSEQ-                                          
050800     05  FILLER                  PIC X.                                   
050900     EJECT                                                                
051000*01  -COPY W0008  -PRE INLC-                                              
051100     05  FILLER                  PIC X.                                   
051200     EJECT                                                                
051300*01  -COPY W0008  -PRE PLAA-                                              
051400     05  FILLER                  PIC X.                                   
051500     EJECT                                                                
051600*01  -COPY W0008  -PRE LOPA-                                              
051700     05  FILLER                  PIC X.                                   
051800     EJECT                                                                
051900*01  -COPY W0008  -PRE KVAE-                                              
052000     05  FILLER                  PIC X.                                   
052100     EJECT                                                                
052200*01  -COPY W0008  -PRE KVABSEQ-                                           
052300     05  FILLER                  PIC X.                                   
052400     EJECT                                                                
052500*01  -COPY W0008  -PRE WDK6-                                              
052600     05  FILLER                  PIC X.                                   
052700     EJECT                                                                
052800*01  -COPY W0008  -PRE WDB6-                                              
052900     05  FILLER                  PIC X.                                   
053000     EJECT                                                                
053100 01  PMRK-INLA1-PCB              PIC X.                                   
053200                                                                          
053300 01  PMRK-INLA2-PCB              PIC X.                                   
053400                                                                          
053500 01  PMRK-PLAA-PCB               PIC X.                                   
053600                                                                          
053700 01  KOM-KOMA-PCB                PIC X.                                   
053800                                                                          
053900 01  STYR-HANA-PCB               PIC X.                                   
054000                                                                          
054100 01  STYR-PLAA-PCB               PIC X.                                   
054200                                                                          
054300*01  -COPY W0008  -PRE UPFA-                                              
054400     05  FILLER                  PIC X.                                   
054500     EJECT                                                                
054600 PROCEDURE DIVISION  USING MSG-PCB 6191-PCB 6194-PCB 6195-PCB             
054700                           6197-PCB 6202-PCB DISP-PCB USEA-PCB            
054800                           INLA-PCB INLABSEQ-PCB INLC-PCB                 
054900                           PLAA-PCB LOPA-PCB KVAE-PCB                     
055000                           KVABSEQ-PCB WDK6-PCB WDB6-PCB                  
055100                           PMRK-INLA1-PCB PMRK-INLA2-PCB                  
055200                           PMRK-PLAA-PCB KOM-KOMA-PCB                     
055300                           STYR-HANA-PCB                                  
055400                           STYR-PLAA-PCB UPFA-PCB.                        
055500                                                                          
055600     ENTRY 'DLITCBL' USING MSG-PCB 6191-PCB 6194-PCB 6195-PCB             
055700                           6197-PCB 6202-PCB DISP-PCB USEA-PCB            
055800                           INLA-PCB INLABSEQ-PCB INLC-PCB                 
055900                           PLAA-PCB LOPA-PCB KVAE-PCB                     
056000                           KVABSEQ-PCB WDK6-PCB WDB6-PCB                  
056100                           PMRK-INLA1-PCB PMRK-INLA2-PCB                  
056200                           PMRK-PLAA-PCB KOM-KOMA-PCB                     
056300                           STYR-HANA-PCB                                  
056400                           STYR-PLAA-PCB UPFA-PCB.                        
056500                                                                          
056600                                                                          
056700     PERFORM IMS-GET-MSG                                                  
056800     IF SEGMENT-FINNS                                                     
056900       PERFORM A-INIT                                                     
057000       PERFORM B-INIT-KEYS                                                
057200       IF MFS-UPDATE OR MFS-UPD-V                                         
057310         SET REQU-UPDATE TO TRUE                                          
057700       ELSE                                                               
057800         IF MFS-FIRST                                                     
057810           SET REQU-FIRST TO TRUE                                         
058000         ELSE                                                             
058010           SET REQU-QUERY TO TRUE                                         
058100           PERFORM E-SAMMA-SIDA                                           
058200         END-IF                                                           
058300       END-IF                                                             
058400       PERFORM F-CALL-BIZ-LOGIC-W6013610                                  
058800       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O13601 + 4                      
058900       PERFORM IMS-INSERT-MSG                                             
059000     END-IF                                                               
059100                                                                          
059200     MOVE ZERO TO RETURN-CODE                                             
059300     GOBACK                                                               
059400     .                                                                    
059500     EJECT                                                                
059600 A-INIT SECTION.                                                          
059700                                                                          
059800     IF MSG-DUBBLA-TRANSKODER                                             
059900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I13601                 
060000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
060100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
060200     ELSE                                                                 
060300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I13601                  
060400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
060500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
060600     END-IF                                                               
060700                                                                          
060800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
060900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
061000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
061100                                                                          
061200     MOVE LOW-VALUE TO MSG-AREA                                           
061300     MOVE 'W6O136N1' TO MFS-IDMOD                                         
061400     MOVE '6136' TO MOD-IDTRANS                                           
061500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
061600                                                                          
061700     IF EGEN-MID OR HELP-MID                                              
061800       CONTINUE                                                           
061900     ELSE                                                                 
062000       MOVE SPACE TO MFS-KDTRTYP                                          
062100       MOVE '7' TO MFS-IDPFK                                              
062200     END-IF                                                               
062300                                                                          
062400     PERFORM AA-INIT-NYCKLAR                                              
063300     .                                                                    
063400     EJECT                                                                
063500 AA-INIT-NYCKLAR SECTION.                                                 
063600                                                                          
063700     MOVE ALL '+' TO MSGI-WMSGINIT                                        
063800     MOVE '001'                  TO MSGI-KDCALL                           
063900     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
064000     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
064100     MOVE '6136'                 TO MSGI-IDTRANS                          
064200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
064300     .                                                                    
064400     EJECT                                                                
064500 B-INIT-KEYS SECTION.                                                     
064600                                                                          
064900     MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-IN                              
065000                             MOD-IDDC-IN                                  
065100                                                                          
065200     IF MID-IDLOPNRM-IN =  ALL '+'                                        
065300       MOVE MID-IDLOPNRM-UT TO WS-IDLOPNRM                                
065400       INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO                
065500     ELSE                                                                 
065600       MOVE MID-IDLOPNRM-IN TO WS-IDLOPNRM                                
065700       MOVE '7'             TO MFS-IDPFK                                  
065800       MOVE SPACE           TO MFS-KDTRTYP                                
065900     END-IF                                                               
065910     MOVE WS-IDLOPNRM       TO REQU-IDLOPNRM-KEY                          
066000                                                                          
066700     IF MID-IDDC-IN =  ALL '+'                                            
066800       MOVE MSGI-IDDC   TO REQU-IDDC-KEY                                  
066900     ELSE                                                                 
067000       MOVE MID-IDDC-IN TO REQU-IDDC-KEY                                  
067100       MOVE '7'         TO MFS-IDPFK                                      
067200       MOVE SPACE       TO MFS-KDTRTYP                                    
067300     END-IF                                                               
068200                                                                          
068300     IF GODK-MID OR HELP-MID                                              
068460       PERFORM BA-FLYTTA-OEVRIGA-NYCKLAR                                  
068500       MOVE WS-IDLOPNRM         TO MOD-IDLOPNRM-UT                        
068600       INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE            
068700       MOVE REQU-IDDC-KEY       TO MOD-IDDC-UT                            
068800       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
068900     ELSE                                                                 
069000       MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-UT                            
069100                               MOD-IDDC-UT                                
069200     END-IF                                                               
070100     .                                                                    
070200     EJECT                                                                
078500 BA-FLYTTA-OEVRIGA-NYCKLAR SECTION.                                       
078501     IF MID-ADINLOMR-PRT = ALL '+'                                        
078502       CONTINUE                                                           
078503     ELSE                                                                 
078504       MOVE MID-ADINLOMR-PRT TO MOD-ADINLOMR-PRT                          
078505     END-IF                                                               
078506                                                                          
078507     MOVE MFS-RENSA-FAELT    TO MOD-IDINLVGN-IN                           
078508     IF MID-IDINLVGN-IN = ALL '+'                                         
078509       MOVE MID-IDINLVGN-UT  TO WS-IDINLVGN                               
078510     ELSE                                                                 
078511       MOVE MID-IDINLVGN-IN  TO WS-IDINLVGN                               
078512     END-IF                                                               
078513     MOVE WS-IDINLVGN        TO MOD-IDINLVGN-UT                           
078514                                                                          
078515     MOVE MFS-RENSA-FAELT    TO MOD-ADINLOMR-IN                           
078516     IF MID-ADINLOMR-IN = ALL '+'                                         
078517       MOVE MID-ADINLOMR-UT  TO WS-ADINLOMR                               
078518     ELSE                                                                 
078519       MOVE MID-ADINLOMR-IN  TO WS-ADINLOMR                               
078520     END-IF                                                               
078521     MOVE WS-ADINLOMR        TO MOD-ADINLOMR-UT                           
078522                                                                          
078523     MOVE MFS-RENSA-FAELT    TO MOD-ADINLOMR-NXT-IN                       
078524     IF MID-ADINLOMR-NXT-IN = ALL '+'                                     
078525       MOVE MID-ADINLOMR-NXT-UT TO WS-ADINLOMR-NXT                        
078526     ELSE                                                                 
078527       MOVE MID-ADINLOMR-NXT-IN TO WS-ADINLOMR-NXT                        
078528     END-IF                                                               
078529     MOVE WS-ADINLOMR-NXT    TO MOD-ADINLOMR-NXT-UT                       
078530                                                                          
078531     MOVE MFS-RENSA-FAELT    TO MOD-KDINLQ-IN                             
078532     IF MID-KDINLQ-IN = ALL '+'                                           
078533       MOVE MID-KDINLQ-UT    TO WS-KDINLQ                                 
078534     ELSE                                                                 
078535       MOVE MID-KDINLQ-IN    TO WS-KDINLQ                                 
078536     END-IF                                                               
078537     MOVE WS-KDINLQ          TO MOD-KDINLQ-UT                             
078538                                                                          
078539     MOVE MFS-RENSA-FAELT    TO MOD-BEFT-FOM-IN                           
078540     IF MID-BEFT-FOM-IN = ALL '+'                                         
078541       MOVE MID-BEFT-FOM-UT  TO WS-BEFT-FOM                               
078542     ELSE                                                                 
078543       MOVE MID-BEFT-FOM-IN  TO WS-BEFT-FOM                               
078544     END-IF                                                               
078545     MOVE WS-BEFT-FOM        TO MOD-BEFT-FOM-UT                           
078546                                                                          
078547     MOVE MFS-RENSA-FAELT    TO MOD-BEFT-TOM-IN                           
078548     IF MID-BEFT-TOM-IN = ALL '+'                                         
078549       MOVE MID-BEFT-TOM-UT  TO WS-BEFT-TOM                               
078550     ELSE                                                                 
078551       MOVE MID-BEFT-TOM-IN  TO WS-BEFT-TOM                               
078552     END-IF                                                               
078553     MOVE WS-BEFT-TOM        TO MOD-BEFT-TOM-UT                           
078554                                                                          
078555     MOVE MFS-RENSA-FAELT    TO MOD-FLINLFB-IN                            
078556     IF MID-FLINLFB-IN = ALL '+'                                          
078557       MOVE MID-FLINLFB-UT   TO WS-FLINLFB                                
078558     ELSE                                                                 
078559       MOVE MID-FLINLFB-IN   TO WS-FLINLFB                                
078560     END-IF                                                               
078561     MOVE WS-FLINLFB         TO MOD-FLINLFB-UT                            
078562                                                                          
078563     MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-KOLLI-IN                      
078564     IF MID-IDLEVNR-KOLLI-IN = ALL '+'                                    
078565       MOVE MID-IDLEVNR-KOLLI-UT TO WS-IDLEVNR-KOLLI                      
078566     ELSE                                                                 
078567       MOVE MID-IDLEVNR-KOLLI-IN TO WS-IDLEVNR-KOLLI                      
078568     END-IF                                                               
078569     MOVE WS-IDLEVNR-KOLLI   TO MOD-IDLEVNR-KOLLI-UT                      
078570                                                                          
078571     MOVE MFS-RENSA-FAELT    TO MOD-IDOKOLLI-IN                           
078572     IF MID-IDOKOLLI-IN = ALL '+'                                         
078573       MOVE MID-IDOKOLLI-UT  TO WS-IDOKOLLI                               
078574     ELSE                                                                 
078575       MOVE MID-IDOKOLLI-IN  TO WS-IDOKOLLI                               
078576     END-IF                                                               
078577     MOVE WS-IDOKOLLI        TO MOD-IDOKOLLI-UT                           
078578     .                                                                    
078579     EJECT                                                                
078580 E-SAMMA-SIDA SECTION.                                                    
078600                                                                          
078700     IF EGEN-MID OR HELP-MID                                              
078800       CONTINUE                                                           
081400     ELSE                                                                 
081500       PERFORM MFS-RENSA-FAELT-IN                                         
081600     END-IF                                                               
081700     .                                                                    
081800     EJECT                                                                
081900 F-CALL-BIZ-LOGIC-W6013610 SECTION.                                       
082000                                                                          
082100     PERFORM FA-INIT-REQU                                                 
082200                                                                          
082300     CALL W6013610 USING REQU-AREA RESP-AREA MAX-KVRADER                  
082400                         MSG-PCB 6191-PCB 6194-PCB 6195-PCB               
082710                         6197-PCB 6202-PCB DISP-PCB USEA-PCB              
082720                         INLA-PCB INLABSEQ-PCB INLC-PCB                   
082730                         PLAA-PCB LOPA-PCB KVAE-PCB                       
082740                         KVABSEQ-PCB WDK6-PCB WDB6-PCB                    
082750                         PMRK-INLA1-PCB PMRK-INLA2-PCB                    
082760                         PMRK-PLAA-PCB KOM-KOMA-PCB                       
082770                         STYR-HANA-PCB                                    
082780                         STYR-PLAA-PCB UPFA-PCB                           
082800                                                                          
082900     PERFORM FB-SET-MSG-AND-HILIGHT                                       
083300     IF NOT WRONG-KEY                                                     
083400        PERFORM FC-MOVE-RESP-TO-MOD                                       
083500     END-IF                                                               
084000     .                                                                    
084100     EJECT                                                                
084200 FA-INIT-REQU SECTION.                                                    
084300                                                                          
084400     MOVE MAX-KVRADER               TO REQU-KVRADER                       
084500                                                                          
084540     MOVE MID-FLPREPRA              TO REQU-FLPREPRA                      
085540     MOVE MID-KVAVIS-MOT            TO REQU-KVAVIS-MOT                    
085555     MOVE MID-IDANSTNR              TO REQU-IDANSTNR                      
085561     MOVE MID-FLSVS                 TO REQU-FLSVS                         
085567     MOVE MID-ADINLOMR-PRT          TO REQU-ADINLOMR-PRT                  
085569                                                                          
085573     MOVE +1                        TO INDX                               
085574     PERFORM UNTIL INDX > MAX-INDX                                        
085580       MOVE MID-KDFLETI(INDX)       TO REQU-KDFLETI-LINE(INDX)            
085591       MOVE MID-KVFLETI(INDX)       TO REQU-KVFLETI-LINE(INDX)            
085592       MOVE MID-KVINLART(INDX)      TO REQU-KVINLART-LINE1(INDX)          
085593       MOVE MID-KDKLIPRI(INDX)      TO REQU-KDKLIPRI-LINE(INDX)           
085594       MOVE MID-FLSATS(INDX)        TO REQU-FLSATS-LINE(INDX)             
085595       MOVE MID-FLPREPKL(INDX)      TO REQU-FLPREPKL-LINE(INDX)           
085596       ADD +1                       TO INDX                               
085597     END-PERFORM                                                          
085598                                                                          
085600     MOVE '101'                     TO REQU-IDMSGVER                      
085700     MOVE MSGI-IDUSER               TO REQU-IDUSER                        
085800     MOVE MSGI-IDSPRAK              TO REQU-IDSPRAK                       
085900     .                                                                    
086000 FB-SET-MSG-AND-HILIGHT SECTION.                                          
086100                                                                          
086200     MOVE RESP-IDMSG-ERROR          TO MCNV-IDMSG-ERROR                   
086300                                       W-IDMSG-ERROR                      
086400     MOVE RESP-IDMSG-INFO           TO MCNV-IDMSG-INFO                    
086500     MOVE RESP-IDELMT-ERROR         TO MCNV-IDELMT-ERROR                  
086600     MOVE MSGI-IDSPRAK              TO MCNV-IDSPRAK                       
086700                                                                          
086800     CALL WL01MCNV USING MCNV-AREA                                        
086900                                                                          
087000     MOVE MCNV-MFSINF               TO MOD-TEMFSINF                       
087100     MOVE MCNV-MFSFEL               TO MOD-TEMFSFEL                       
087101                                                                          
087102     IF MOD-TEMFSFEL = SPACES                                             
087103     AND RESP-BEPRTLST NOT = ALL '+'                                      
087104        MOVE RESP-BEPRTLST          TO MOD-TEMFSFEL                       
087105     END-IF                                                               
087110                                                                          
087200     .                                                                    
087300     EJECT                                                                
087400 FC-MOVE-RESP-TO-MOD SECTION.                                             
087500                                                                          
087510     MOVE RESP-IDARTNR-ATTR              TO MOD-IDARTNR-ATTR              
087520     MOVE RESP-KVAVIS-ATTR               TO MOD-KVAVIS-ATTR               
087530     MOVE RESP-BEART-ATTR                TO MOD-BEART-ATTR                
087540     MOVE RESP-KDSORT-ATTR               TO MOD-KDSORT-ATTR               
087550     MOVE RESP-BEFT-ATTR                 TO MOD-BEFT-ATTR                 
087560     MOVE RESP-BEFARLIG-ATTR             TO MOD-BEFARLIG-ATTR             
087570     MOVE RESP-KVAVIS-KVAR-ATTR          TO MOD-KVAVIS-KVAR-ATTR          
087580     MOVE RESP-KVAVIS-FPK-KVAR-ATTR      TO                               
087581                                         MOD-KVAVIS-FPK-KVAR-ATTR         
087590     MOVE RESP-KVAVIS-PRIO-KVAR-ATTR     TO                               
087591                                         MOD-KVAVIS-PRIO-KVAR-ATTR        
087592     MOVE RESP-KDLAGEMB-ATTR             TO MOD-KDLAGEMB-ATTR             
087593     MOVE RESP-ADLAGOMR-ATTR             TO MOD-ADLAGOMR-ATTR             
087594     MOVE RESP-ADGANG-ATTR               TO MOD-ADGANG-ATTR               
087595     MOVE RESP-ADPLATS-ATTR              TO MOD-ADPLATS-ATTR              
087596     MOVE RESP-FLKVAANT-TOT-ATTR         TO MOD-FLKVAANT-TOT-ATTR         
087597     MOVE RESP-KVAVIS-KIT-KVAR-ATTR      TO                               
087598                                         MOD-KVAVIS-KIT-KVAR-ATTR         
087599     MOVE RESP-ADINLOMR-PRT-ATTR         TO MOD-ADINLOMR-PRT-ATTR         
087600     MOVE RESP-FLSVS-ATTR                TO MOD-FLSVS-ATTR                
087601                                                                          
087602     MOVE RESP-FLPREPRA-ATTR             TO MOD-FLPREPRA-IN-ATTR          
087603     MOVE RESP-KVAVIS-MOT-ATTR           TO MOD-KVAVIS-MOT-IN-ATTR        
087604     MOVE RESP-IDANSTNR-ATTR             TO MOD-IDANSTNR-IN-ATTR          
087606                                                                          
087610     IF RESP-IDLOPNRM = SPACE                                             
087700        MOVE MFS-ERASE-FIELD              TO MOD-IDLOPNRM-UT              
087800     ELSE                                                                 
087900       IF RESP-IDLOPNRM = ALL '+'                                         
088000          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDLOPNRM-UT              
088100       ELSE                                                               
088200          MOVE RESP-IDLOPNRM              TO MOD-IDLOPNRM-UT              
088300       END-IF                                                             
088400     END-IF                                                               
088401                                                                          
088492     IF RESP-IDARTNR = SPACE                                              
088493        MOVE MFS-ERASE-FIELD              TO MOD-IDARTNR                  
088494     ELSE                                                                 
088495       IF RESP-IDARTNR   =  ALL '+'                                       
088496          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDARTNR                  
088497       ELSE                                                               
088498          MOVE RESP-IDARTNR               TO MOD-IDARTNR                  
088499       END-IF                                                             
088500     END-IF                                                               
088501                                                                          
088502     IF RESP-KVAVIS = SPACE                                               
088503        MOVE MFS-ERASE-FIELD              TO MOD-KVAVIS                   
088504     ELSE                                                                 
088505       IF RESP-KVAVIS = ALL '+'                                           
088506          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVAVIS                   
088507       ELSE                                                               
088508          MOVE RESP-KVAVIS                TO MOD-KVAVIS                   
088509       END-IF                                                             
088510     END-IF                                                               
088511                                                                          
088512     IF RESP-BEART = SPACE                                                
088513        MOVE MFS-ERASE-FIELD              TO MOD-BEART                    
088514     ELSE                                                                 
088515       IF RESP-BEART = ALL '+'                                            
088516          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-BEART                    
088517       ELSE                                                               
088518          MOVE RESP-BEART                 TO MOD-BEART                    
088519       END-IF                                                             
088520     END-IF                                                               
088521                                                                          
088522     IF RESP-KDSORT = SPACE                                               
088523        MOVE MFS-ERASE-FIELD              TO MOD-KDSORT                   
088524     ELSE                                                                 
088525       IF RESP-KDSORT = ALL '+'                                           
088526          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDSORT                   
088527       ELSE                                                               
088528          MOVE RESP-KDSORT                TO MOD-KDSORT                   
088529       END-IF                                                             
088530     END-IF                                                               
088531                                                                          
088532     IF RESP-BEFT = SPACE                                                 
088533        MOVE MFS-ERASE-FIELD              TO MOD-BEFT                     
088534     ELSE                                                                 
088535       IF RESP-BEFT = ALL '+'                                             
088536          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-BEFT                     
088537       ELSE                                                               
088538          MOVE RESP-BEFT                  TO MOD-BEFT                     
088539       END-IF                                                             
088540     END-IF                                                               
088541                                                                          
088552     IF RESP-KVAVIS-KVAR = SPACE                                          
088553        MOVE MFS-ERASE-FIELD              TO MOD-KVAVIS-KVAR              
088554     ELSE                                                                 
088555       IF RESP-KVAVIS-KVAR = ALL '+'                                      
088556          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVAVIS-KVAR              
088557       ELSE                                                               
088558          MOVE RESP-KVAVIS-KVAR           TO MOD-KVAVIS-KVAR              
088559       END-IF                                                             
088560     END-IF                                                               
088561                                                                          
088562     IF RESP-BEFARLIG = SPACE                                             
088563        MOVE MFS-ERASE-FIELD              TO MOD-BEFARLIG                 
088564     ELSE                                                                 
088565       IF RESP-BEFARLIG  = ALL '+'                                        
088566          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-BEFARLIG                 
088567       ELSE                                                               
088568          MOVE RESP-BEFARLIG              TO MOD-BEFARLIG                 
088569       END-IF                                                             
088570     END-IF                                                               
088571                                                                          
088572     IF RESP-KVAVIS-PRIO-KVAR = SPACE                                     
088573        MOVE MFS-ERASE-FIELD              TO MOD-KVAVIS-PRIO-KVAR         
088574     ELSE                                                                 
088575       IF RESP-KVAVIS-PRIO-KVAR = ALL '+'                                 
088576          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVAVIS-PRIO-KVAR         
088577       ELSE                                                               
088578          MOVE RESP-KVAVIS-PRIO-KVAR      TO MOD-KVAVIS-PRIO-KVAR         
088579       END-IF                                                             
088580     END-IF                                                               
088581                                                                          
088583     IF RESP-KVAVIS-FPK-KVAR = SPACE                                      
088584        MOVE MFS-ERASE-FIELD              TO MOD-KVAVIS-FPK-KVAR          
088585     ELSE                                                                 
088586       IF RESP-KVAVIS-FPK-KVAR = ALL '+'                                  
088587          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVAVIS-FPK-KVAR          
088588       ELSE                                                               
088589          MOVE RESP-KVAVIS-FPK-KVAR      TO MOD-KVAVIS-FPK-KVAR           
088590       END-IF                                                             
088591     END-IF                                                               
088592                                                                          
088593     IF RESP-KDLAGEMB = SPACE                                             
088594        MOVE MFS-ERASE-FIELD              TO MOD-KDLAGEMB                 
088595     ELSE                                                                 
088596       IF RESP-KDLAGEMB  = ALL '+'                                        
088597          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KDLAGEMB                 
088598       ELSE                                                               
088599          MOVE RESP-KDLAGEMB              TO MOD-KDLAGEMB                 
088600       END-IF                                                             
088601     END-IF                                                               
088602                                                                          
088603     IF RESP-ADLAGOMR = SPACE                                             
088604        MOVE MFS-ERASE-FIELD              TO MOD-ADLAGOMR                 
088605     ELSE                                                                 
088606       IF RESP-ADLAGOMR  = ALL '+'                                        
088607          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADLAGOMR                 
088608       ELSE                                                               
088609          MOVE RESP-ADLAGOMR              TO MOD-ADLAGOMR                 
088610       END-IF                                                             
088611     END-IF                                                               
088612                                                                          
088613     IF RESP-ADGANG = SPACE                                               
088614        MOVE MFS-ERASE-FIELD              TO MOD-ADGANG                   
088615     ELSE                                                                 
088616       IF RESP-ADGANG = ALL '+'                                           
088617          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADGANG                   
088618       ELSE                                                               
088619          MOVE RESP-ADGANG                TO MOD-ADGANG                   
088620       END-IF                                                             
088621     END-IF                                                               
088622                                                                          
088623     IF RESP-ADPLATS = SPACE                                              
088624        MOVE MFS-ERASE-FIELD              TO MOD-ADPLATS                  
088625     ELSE                                                                 
088626       IF RESP-ADPLATS = ALL '+'                                          
088627          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADPLATS                  
088628       ELSE                                                               
088629          MOVE RESP-ADPLATS               TO MOD-ADPLATS                  
088630       END-IF                                                             
088631     END-IF                                                               
088632                                                                          
088633     IF RESP-FLKVAANT-TOT = SPACE                                         
088634        MOVE MFS-ERASE-FIELD              TO MOD-FLKVAANT-TOT             
088635     ELSE                                                                 
088636       IF RESP-FLKVAANT-TOT = ALL '+'                                     
088637          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLKVAANT-TOT             
088638       ELSE                                                               
088639          MOVE RESP-FLKVAANT-TOT          TO MOD-FLKVAANT-TOT             
088640       END-IF                                                             
088641     END-IF                                                               
088642                                                                          
088663                                                                          
088664     IF RESP-KVAVIS-KIT-KVAR = SPACE                                      
088665        MOVE MFS-ERASE-FIELD              TO MOD-KVAVIS-KIT-KVAR          
088666     ELSE                                                                 
088667       IF RESP-KVAVIS-KIT-KVAR = ALL '+'                                  
088668          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVAVIS-KIT-KVAR          
088669       ELSE                                                               
088670          MOVE RESP-KVAVIS-KIT-KVAR       TO MOD-KVAVIS-KIT-KVAR          
088671       END-IF                                                             
088672     END-IF                                                               
088673                                                                          
088674     IF RESP-ADTRDEST-KIT = SPACE                                         
088675        MOVE MFS-ERASE-FIELD              TO MOD-ADTRDEST-KIT             
088676     ELSE                                                                 
088677       IF RESP-ADTRDEST-KIT = ALL '+'                                     
088678          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADTRDEST-KIT             
088679       ELSE                                                               
088680          MOVE RESP-ADTRDEST-KIT          TO MOD-ADTRDEST-KIT             
088681       END-IF                                                             
088682     END-IF                                                               
088683                                                                          
088713     IF RESP-ADINLOMR-PRT = SPACE                                         
088714        MOVE MFS-ERASE-FIELD              TO MOD-ADINLOMR-PRT             
088715     ELSE                                                                 
088716       IF RESP-ADINLOMR-PRT = ALL '+'                                     
088717          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-ADINLOMR-PRT             
088718       ELSE                                                               
088719          MOVE RESP-ADINLOMR-PRT          TO MOD-ADINLOMR-PRT             
088720       END-IF                                                             
088721     END-IF                                                               
088722                                                                          
088723     IF RESP-KVROS = SPACE                                                
088724        MOVE MFS-ERASE-FIELD              TO MOD-KVROS                    
088725     ELSE                                                                 
088726       IF RESP-KVROS = ALL '+'                                            
088727          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVROS                    
088728       ELSE                                                               
088729          MOVE RESP-KVROS                 TO MOD-KVROS                    
088730       END-IF                                                             
088731     END-IF                                                               
088732                                                                          
088746     IF RESP-FLSVS = SPACE                                                
088747        MOVE MFS-ERASE-FIELD              TO MOD-FLSVS                    
088748     ELSE                                                                 
088749       IF RESP-FLSVS = ALL '+'                                            
088750          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLSVS                    
088751       ELSE                                                               
088752          MOVE RESP-FLSVS                 TO MOD-FLSVS                    
088753       END-IF                                                             
088760     END-IF                                                               
088852                                                                          
088854     IF RESP-FLPREPRA = SPACE                                             
088855        MOVE MFS-ERASE-FIELD              TO MOD-FLPREPRA-IN              
088856     ELSE                                                                 
088857       IF RESP-FLPREPRA = ALL '+'                                         
088858          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLPREPRA-IN              
088859       ELSE                                                               
088860          MOVE RESP-FLPREPRA              TO MOD-FLPREPRA-IN              
088861       END-IF                                                             
088862     END-IF                                                               
088864                                                                          
088865     IF RESP-IDANSTNR = SPACE                                             
088866        MOVE MFS-ERASE-FIELD              TO MOD-IDANSTNR-IN              
088867     ELSE                                                                 
088868       IF RESP-IDANSTNR = ALL '+'                                         
088869          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDANSTNR-IN              
088870       ELSE                                                               
088871          MOVE RESP-IDANSTNR              TO MOD-IDANSTNR-IN              
088872       END-IF                                                             
088873     END-IF                                                               
088874                                                                          
088875     IF RESP-KVAVIS-MOT = SPACE                                           
088876        MOVE MFS-ERASE-FIELD              TO MOD-KVAVIS-MOT-IN            
088877     ELSE                                                                 
088878       IF RESP-KVAVIS-MOT = ALL '+'                                       
088879          MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVAVIS-MOT-IN            
088880       ELSE                                                               
088881          MOVE RESP-KVAVIS-MOT            TO MOD-KVAVIS-MOT-IN            
088882       END-IF                                                             
088883     END-IF                                                               
088884     MOVE +1  TO INDX                                                     
088885     PERFORM UNTIL INDX > RESP-KVRADER                                    
088886                                                                          
088887       MOVE RESP-KDFLETI-LINE-ATTR(INDX)  TO                              
088888                                        MOD-KDFLETI-IN-ATTR(INDX)         
088889       MOVE RESP-KVFLETI-LINE-ATTR(INDX)  TO                              
088890                                        MOD-KVFLETI-IN-ATTR(INDX)         
088891       MOVE RESP-KVINLART-LINE1-ATTR(INDX) TO                             
088892                                        MOD-KVINLART-IN-ATTR(INDX)        
088893       MOVE RESP-KDKLIPRI-LINE-ATTR(INDX) TO                              
088894                                        MOD-KDKLIPRI-IN-ATTR(INDX)        
088895       MOVE RESP-FLSATS-LINE-ATTR(INDX)   TO                              
088896                                        MOD-FLSATS-IN-ATTR(INDX)          
088897       MOVE RESP-FLPREPKL-LINE-ATTR(INDX) TO                              
088898                                        MOD-FLPREPKL-IN-ATTR(INDX)        
088899       IF RESP-KDFLETI-LINE(INDX) = SPACE                                 
088900          MOVE MFS-ERASE-FIELD              TO                            
088901                                        MOD-KDFLETI-IN(INDX)              
088902       ELSE                                                               
088903          IF RESP-KDFLETI-LINE(INDX) = ALL '+'                            
088904             MOVE MFS-DO-NOT-TOUCH-FIELD    TO                            
088905                                        MOD-KDFLETI-IN(INDX)              
088906          ELSE                                                            
088907             MOVE RESP-KDFLETI-LINE(INDX)   TO                            
088908                                        MOD-KDFLETI-IN(INDX)              
088909          END-IF                                                          
088910       END-IF                                                             
088911                                                                          
088912       IF RESP-KVFLETI-LINE(INDX) = SPACE                                 
088913          MOVE MFS-ERASE-FIELD            TO                              
088914                                     MOD-KVFLETI-IN(INDX)                 
088915       ELSE                                                               
088916          IF RESP-KVFLETI-LINE(INDX) = ALL '+'                            
088917             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
088918                                     MOD-KVFLETI-IN(INDX)                 
088919          ELSE                                                            
088920             MOVE RESP-KVFLETI-LINE(INDX) TO                              
088921                                     MOD-KVFLETI-IN(INDX)                 
088922          END-IF                                                          
088923       END-IF                                                             
088924                                                                          
088925       IF RESP-KVINLART-LINE1(INDX) = SPACE                               
088926          MOVE MFS-ERASE-FIELD            TO                              
088927                                     MOD-KVINLART-IN(INDX)                
088928       ELSE                                                               
088929          IF RESP-KVINLART-LINE1(INDX) = ALL '+'                          
088930             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
088931                                     MOD-KVINLART-IN(INDX)                
088932          ELSE                                                            
088933             MOVE RESP-KVINLART-LINE1(INDX) TO                            
088934                                     MOD-KVINLART-IN(INDX)                
088935          END-IF                                                          
088936       END-IF                                                             
088937                                                                          
088938       IF RESP-KDKLIPRI-LINE(INDX) = SPACE                                
088939          MOVE MFS-ERASE-FIELD            TO                              
088940                                     MOD-KDKLIPRI-IN(INDX)                
088941       ELSE                                                               
088942          IF RESP-KDKLIPRI-LINE(INDX) = ALL '+'                           
088943             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
088944                                     MOD-KDKLIPRI-IN(INDX)                
088945          ELSE                                                            
088946             MOVE RESP-KDKLIPRI-LINE(INDX) TO                             
088947                                     MOD-KDKLIPRI-IN(INDX)                
088948          END-IF                                                          
088949       END-IF                                                             
088950                                                                          
088951       IF RESP-FLSATS-LINE(INDX) = SPACE                                  
088952          MOVE MFS-ERASE-FIELD            TO                              
088953                                     MOD-FLSATS-IN(INDX)                  
088954       ELSE                                                               
088955          IF RESP-FLSATS-LINE(INDX)   = ALL '+'                           
088956             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
088957                                     MOD-FLSATS-IN(INDX)                  
088958          ELSE                                                            
088959             MOVE RESP-FLSATS-LINE(INDX)                                  
088960                                          TO                              
088961                                     MOD-FLSATS-IN(INDX)                  
088962          END-IF                                                          
088963       END-IF                                                             
088964                                                                          
088965       IF RESP-FLPREPKL-LINE(INDX) = SPACE                                
088966          MOVE MFS-ERASE-FIELD            TO                              
088967                                     MOD-FLPREPKL-IN(INDX)                
088968       ELSE                                                               
088969          IF RESP-FLPREPKL-LINE(INDX) = ALL '+'                           
088970             MOVE MFS-DO-NOT-TOUCH-FIELD  TO                              
088971                                     MOD-FLPREPKL-IN(INDX)                
088972          ELSE                                                            
088973             MOVE RESP-FLPREPKL-LINE(INDX)                                
088974                                          TO                              
088975                                     MOD-FLPREPKL-IN(INDX)                
088976          END-IF                                                          
088977       END-IF                                                             
088978                                                                          
088979       ADD +1 TO INDX                                                     
088980     END-PERFORM                                                          
088981                                                                          
088982     PERFORM UNTIL INDX > MAX-KVRADER                                     
088983       MOVE MFS-ERASE-FIELD TO MOD-KVINLART-IN(INDX)                      
088984                               MOD-KDFLETI-IN(INDX)                       
088985                               MOD-KVFLETI-IN(INDX)                       
088986                               MOD-KDKLIPRI-IN(INDX)                      
088987                               MOD-FLSATS-IN(INDX)                        
088988                               MOD-FLPREPKL-IN(INDX)                      
088989                                                                          
088990       ADD +1 TO INDX                                                     
088991     END-PERFORM                                                          
088992     .                                                                    
089000     EJECT                                                                
238300 MFS-RENSA-FAELT-UT SECTION.                                              
238400                                                                          
238500*    --- ALLA UTDATA-FÄLT                                                 
238600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR                                  
238700                             MOD-KVAVIS                                   
238800                             MOD-BEART                                    
238900                             MOD-KDSORT                                   
239000                             MOD-BEFT                                     
239100                             MOD-BEFARLIG                                 
239200                             MOD-KVAVIS-KVAR                              
239300                             MOD-KVAVIS-FPK-KVAR                          
239400                             MOD-KVAVIS-PRIO-KVAR                         
239500                             MOD-KDLAGEMB                                 
239600                             MOD-ADLAGOMR                                 
239700                             MOD-ADGANG                                   
239800                             MOD-ADPLATS                                  
239900                             MOD-FLKVAANT-TOT                             
240000                             MOD-KVAVIS-KIT-KVAR                          
240100                             MOD-ADTRDEST-KIT                             
240200                             MOD-FLSVS                                    
240300     .                                                                    
240400     SKIP2                                                                
240500 MFS-RENSA-FAELT-IN SECTION.                                              
240600                                                                          
240700*    --- ALLA INDATA-FÄLT                                                 
240800     MOVE +1 TO INDX                                                      
240900     PERFORM UNTIL INDX > MAX-INDX                                        
241000       MOVE MFS-RENSA-FAELT TO MOD-KDFLETI-IN(INDX)                       
241100                               MOD-KVFLETI-IN(INDX)                       
241200                               MOD-KVINLART-IN(INDX)                      
241300       ADD +1 TO INDX                                                     
241400     END-PERFORM                                                          
241500     MOVE MFS-RENSA-FAELT TO MOD-KVAVIS-MOT-IN                            
241600                             MOD-IDANSTNR-IN                              
241700                             MOD-FLSVS                                    
241800                                                                          
241900     .                                                                    
242000     EJECT                                                                
250100* --- IMS SEKTIONER ---                                                   
250200     SKIP3                                                                
250300 IMS-GET-MSG SECTION.                                                     
250400                                                                          
250500     MOVE '  QC' TO GODK-STATUSKODER                                      
250600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
250700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
250800     PERFORM IMS-STATUSKONTROLL                                           
250900     .                                                                    
251000     SKIP3                                                                
251100 IMS-INSERT-MSG SECTION.                                                  
251200                                                                          
251300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
251400       MOVE '0' TO MFS-KDHUVOMR                                           
251500     END-IF                                                               
251600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
251700     MOVE SPACE TO GODK-STATUSKODER                                       
251800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
251900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
252000     PERFORM IMS-STATUSKONTROLL                                           
252100     .                                                                    
252200     EJECT                                                                
281300 IMS-STATUSKONTROLL SECTION.                                              
281400                                                                          
281500     SET STATUS-IX TO 1                                                   
281600     SEARCH GODK-STATUS                                                   
281700       AT END                                                             
281800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
281900         DELIMITED BY SIZE INTO FELTEXT                                   
282000         CALL FELLOG                                                      
282100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
282200         CONTINUE                                                         
282300     END-SEARCH                                                           
282400     .                                                                    
