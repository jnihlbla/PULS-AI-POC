000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6013900.                                                
000400*AUTHOR.         GERRY  CARMICHAEL / ARCHANA BHAT.                        
000500*DATE-WRITTEN.   92/09/02 / JUNE 2012.                                    
000600*                                                                         
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        MOTTAGNINGSKONTROLL INLEVERANS                                   
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR W6UPFA (W6L1)                              
001300*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001400*        PROGRAMMET UPPDATERAR W6KVAH (W6D2)                              
001500*        PROGRAMMET UPPDATERAR W6KVAE (W6H7)                              
001600*        PROGRAMMET LÄSER      WDF5                                       
001700*        PROGRAMMET LÄSER      WLLEVA (W6F1)                              
001800*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001900*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002000*        PROGRAMMET LÄSER      W6INLC (W6D1B1)                            
002100*        PROGRAMMET LÄSER              WDB6                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W6T139                                              
002500*                     W6T139U                                             
002600*                     W6T139V                                             
002700*                                                                         
002800*        MID:         W6I13901                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W6O13901                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W6013900'.            
004100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004400 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
005400 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
005500 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
006000*                                                                         
006100                                                                          
006200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006400                                                                          
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  YES                         PIC X       VALUE 'Y'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006710*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006720 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006800                                                                          
007600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007700                                                                          
010800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010900     88  EGEN-MID                            VALUE '6139'.                
011000     88  GODK-MID                            VALUE '6131' '6132'          
011100                                                   '6133' '6134'          
011200                                                   '6135' '6136'          
011300                                                   '6137' '6138'          
011400                                                   '6139'.                
011500     88  HELP-MID                            VALUE '0551'.                
011600     EJECT                                                                
011700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011800 01  GENERELLA-SUBPROGRAM.                                                
012000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012210     03  W6013910                PIC X(8)    VALUE 'W6013910'.            
012220     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
012300     EJECT                                                                
012310 01  W-IDMSG-ERROR               PIC X(3).                                
012320     88   WRONG-KEY                          VALUE '022'.                 
012400*01 -COPY WMSGINIT                                                        
012500     SKIP3                                                                
012600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012700*01 -COPY WMEDAREA                                                        
012800     SKIP3                                                                
015300*                                                                         
015400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015500     SKIP3                                                                
015600*01  MID -COPY W6I13901                                                   
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015900     SKIP3                                                                
016000*01  -COPY WMSGAREA                                                       
016100     EJECT                                                                
016200     03  MOD REDEFINES MSG-AREA.                                          
016300*      05  -COPY W6O13901                                                 
016400     EJECT                                                                
016410 01  REQU-AREA.                                                           
016420*    03 -COPY WZ01REQU                                                    
016430*    03 -COPY W60139I1                                                    
016450     EJECT                                                                
016460 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
016470 01  RESP-AREA.                                                           
016480*    03 -COPY WZ01RESP                                                    
016490*    03 -COPY W60139O1                                                    
016500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016600     SKIP3                                                                
016700*01  -COPY WMFSAREA                                                       
016710 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV'.            
016720     SKIP3                                                                
016730*01  -COPY WL01MCNV                                                       
016800     EJECT                                                                
017900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018000*                                                                         
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018300     SKIP3                                                                
021100*    --- STATUS-KOD FRÅN IMS                                              
021200 01  STATUS-WS                   PIC XX.                                  
021300     88  SEGMENT-FINNS                       VALUE '  '.                  
021400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021600     SKIP2                                                                
021700 01  GODK-STATUSKODER.                                                    
021800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021900     SKIP3                                                                
022300     EJECT                                                                
022400*    --- IMS FUNKTIONSKODER                                               
022500*01  -COPY W0003                                                          
022600     EJECT                                                                
028200 LINKAGE SECTION.                                                         
028300                                                                          
028400*01  -COPY W0009  -PRE MSG-                                               
028500     EJECT                                                                
028600*01  -COPY W0009  -PRE ALT-                                               
028700     EJECT                                                                
028710*01  -COPY W0009  -PRE ALT-IMS-                                           
028720     EJECT                                                                
028800*01  -COPY W0008  -PRE USEA-                                              
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029100*01  -COPY W0008  -PRE UPFA-                                              
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008  -PRE INLA-                                              
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008  -PRE KVAH-                                              
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008  -PRE WDF5-                                              
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008  -PRE LEVA-                                              
030400     05  FILLER                  PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008  -PRE KVAE1-                                             
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900*01  -COPY W0008  -PRE ARTC-                                              
031000     05  FILLER                  PIC X.                                   
031100     EJECT                                                                
031200*01  -COPY W0008  -PRE BENA-                                              
031300     05  FILLER                  PIC X.                                   
031400     EJECT                                                                
031500*01  -COPY W0008  -PRE INLC-                                              
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800*01  -COPY W0008  -PRE KVAE2-                                             
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100*01  -COPY W0008  -PRE WDB6-                                              
032200     05  FILLER                  PIC X.                                   
032300     EJECT                                                                
032400 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB ALT-IMS-PCB                    
032410     USEA-PCB UPFA-PCB                                                    
032500     INLA-PCB KVAH-PCB WDF5-PCB LEVA-PCB KVAE1-PCB ARTC-PCB               
032600     BENA-PCB INLC-PCB KVAE2-PCB WDB6-PCB.                                
032800                                                                          
032900     ENTRY 'DLITCBL' USING MSG-PCB MSG-PCB ALT-PCB ALT-IMS-PCB            
032910     USEA-PCB UPFA-PCB                                                    
033000     INLA-PCB KVAH-PCB WDF5-PCB LEVA-PCB KVAE1-PCB ARTC-PCB               
033100     BENA-PCB INLC-PCB KVAE2-PCB WDB6-PCB.                                
033300                                                                          
033400     PERFORM IMS-GET-MSG                                                  
033500     IF SEGMENT-FINNS                                                     
033600       PERFORM A-INIT                                                     
033700       PERFORM B-INIT-KEYS                                                
034400       IF MFS-UPDATE                                                      
034500         SET REQU-UPDATE           TO TRUE                                
034600         MOVE MID-IDKR-ENTER       TO REQU-IDKR-START                     
034700         MOVE MID-IDKVAINF-ENTER   TO REQU-IDKVAINF-START                 
034900       ELSE                                                               
034910         IF MFS-UPD-V                                                     
034920           SET REQU-UPD-V          TO TRUE                                
034921           MOVE MID-IDKR-ENTER     TO REQU-IDKR-START                     
034922           MOVE MID-IDKVAINF-ENTER TO REQU-IDKVAINF-START                 
034930         ELSE                                                             
035000           IF MFS-FIRST                                                   
035010             SET REQU-FIRST        TO TRUE                                
035100             PERFORM C-FOERSTA-SIDA                                       
035200           ELSE                                                           
035300             IF MFS-NEXT                                                  
035310               SET REQU-NEXT       TO TRUE                                
035400               PERFORM D-NAESTA-SIDA                                      
035500             ELSE                                                         
035510               SET REQU-QUERY      TO TRUE                                
035600               PERFORM E-SAMMA-SIDA                                       
035700             END-IF                                                       
035710           END-IF                                                         
035800         END-IF                                                           
035900       END-IF                                                             
036000       PERFORM F-CALL-BIZ-LOGIC-W6013910                                  
036500       IF RESP-FLAGGA-KR-HOPP = JA OR YES                                 
036510         CONTINUE                                                         
036520       ELSE                                                               
036600         COMPUTE MSG-KVLL = LENGTH OF MOD-W6O13901 + 4                    
036700         PERFORM IMS-INSERT-MSG                                           
036800       END-IF                                                             
036900     END-IF                                                               
037000                                                                          
037100     MOVE ZERO TO RETURN-CODE                                             
037200     GOBACK                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 A-INIT SECTION.                                                          
037600                                                                          
037700     IF MSG-DUBBLA-TRANSKODER                                             
037800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I13901                 
037900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
038000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
038200     ELSE                                                                 
038300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I13901                 
038400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
038500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
038700     END-IF                                                               
038800                                                                          
038900     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
039000     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
039100     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
039200                                                                          
039300     MOVE LOW-VALUE                       TO MSG-AREA                     
039400     MOVE 'W6O139N1'                      TO MFS-IDMOD                    
039500     MOVE '6139'                          TO MOD-IDTRANS                  
039600     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
039700                                             MOD-TEMFSINF                 
039800     IF EGEN-MID OR HELP-MID                                              
039900       CONTINUE                                                           
040000     ELSE                                                                 
040100       MOVE SPACE TO MFS-KDTRTYP                                          
040200       MOVE '7' TO MFS-IDPFK                                              
040300     END-IF                                                               
040400                                                                          
040600                                                                          
040700     PERFORM AA-INIT-NYCKLAR                                              
040800                                                                          
041800     .                                                                    
041900     EJECT                                                                
042000 AA-INIT-NYCKLAR SECTION.                                                 
042100                                                                          
042200     MOVE ALL '+' TO MSGI-WMSGINIT                                        
042300     MOVE '001'                  TO MSGI-KDCALL                           
042400     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
042500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042600     .                                                                    
042700     EJECT                                                                
042800 B-INIT-KEYS SECTION.                                                     
042900                                                                          
043500     MOVE MFS-RENSA-FAELT   TO MOD-IDLOPNRM-IN                            
043600                               MOD-IDDC-IN                                
043700                                                                          
043800     IF MID-IDLOPNRM-IN =  ALL '+'                                        
043900       MOVE MID-IDLOPNRM-UT TO WS-IDLOPNRM                                
044000       INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO                
044100     ELSE                                                                 
044200       MOVE MID-IDLOPNRM-IN TO WS-IDLOPNRM                                
044300       MOVE '7'             TO MFS-IDPFK                                  
044400       MOVE SPACE           TO MFS-KDTRTYP                                
044500     END-IF                                                               
044600     MOVE WS-IDLOPNRM       TO REQU-IDLOPNRM-KEY                          
045300                                                                          
045400     IF MID-IDDC-IN =  ALL '+' OR MID-IDDC-IN = SPACE                     
045500       MOVE MSGI-IDDC       TO REQU-IDDC-KEY                              
045600     ELSE                                                                 
045700       MOVE MID-IDDC-IN     TO REQU-IDDC-KEY                              
045800       MOVE '7'             TO MFS-IDPFK                                  
045900       MOVE SPACE           TO MFS-KDTRTYP                                
046000     END-IF                                                               
046100                                                                          
046900     IF GODK-MID OR HELP-MID                                              
047000       PERFORM BA-FLYTTA-OEVRIGA-NYCKLAR                                  
047100       MOVE WS-IDLOPNRM     TO MOD-IDLOPNRM-UT                            
047200       INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE            
047300       MOVE REQU-IDDC-KEY   TO MOD-IDDC-UT                                
047400       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
047500     ELSE                                                                 
047600       MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-UT                            
047700     END-IF                                                               
047800                                                                          
048600     .                                                                    
048700     EJECT                                                                
048800 BA-FLYTTA-OEVRIGA-NYCKLAR SECTION.                                       
048900                                                                          
055200     MOVE MFS-RENSA-FAELT        TO MOD-IDLEVNR-KOLLI-IN                  
055300     IF MID-IDLEVNR-KOLLI-IN = ALL '+'                                    
055400       MOVE MID-IDLEVNR-KOLLI-UT TO WS-IDLEVNR-KOLLI                      
055500     ELSE                                                                 
055600       MOVE MID-IDLEVNR-KOLLI-IN TO WS-IDLEVNR-KOLLI                      
055700     END-IF                                                               
055800     MOVE WS-IDLEVNR-KOLLI       TO MOD-IDLEVNR-KOLLI-UT                  
055810                                    REQU-IDLEVNR-KOLLI                    
055900                                                                          
056000     MOVE MFS-RENSA-FAELT        TO MOD-IDOKOLLI-IN                       
056100     IF MID-IDOKOLLI-IN = ALL '+'                                         
056200       MOVE MID-IDOKOLLI-UT      TO WS-IDOKOLLI                           
056300     ELSE                                                                 
056400       MOVE MID-IDOKOLLI-IN      TO WS-IDOKOLLI                           
056500     END-IF                                                               
056600     MOVE WS-IDOKOLLI            TO MOD-IDOKOLLI-UT                       
056610                                    REQU-IDOKOLLI                         
056700                                                                          
056710     IF MID-IDLEVNR-KOLLI-IN = ALL '+'                                    
056720       MOVE MID-IDLEVNR-KOLLI-UT TO REQU-IDLEVNR-KOLLI                    
056730     ELSE                                                                 
056740       MOVE MID-IDLEVNR-KOLLI-IN TO REQU-IDLEVNR-KOLLI                    
056750     END-IF                                                               
056760     IF MID-IDOKOLLI-IN = ALL '+'                                         
056770       MOVE MID-IDOKOLLI-UT      TO REQU-IDOKOLLI                         
056780     ELSE                                                                 
056790       MOVE MID-IDOKOLLI-IN      TO REQU-IDOKOLLI                         
056791     END-IF                                                               
056800***** TILLÄGG FÖR ATT RENSA NYCKLAR. OM NYTT PARTINUMMER                  
056900**    ANGES, RENSAS IDLEVNR OCH KOLLI. DETTA FÖR ATT                      
057000**    NYA NYCKLAR SKA HÄNGA MED NÄR MAN HOPPAR TILL BL.A.                 
057100**    6133-BILDEN                                                         
057200**                                                                        
057300*    IF NYCKLAR-OK                                                        
057400*    AND EGEN-MID                                                         
057500     IF MID-IDLOPNRM-IN NOT = ALL '+'                                     
057600        MOVE MFS-RENSA-FAELT     TO MOD-IDLEVNR-KOLLI-UT                  
057700                                    MOD-IDOKOLLI-UT                       
057800     END-IF                                                               
057900*    END-IF                                                               
058000     .                                                                    
058100     EJECT                                                                
058200 C-FOERSTA-SIDA SECTION.                                                  
058300                                                                          
058800*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
058900     MOVE ZERO       TO MOD-IDKR-ENTER                                    
059000                        MOD-IDKR-NEXT                                     
059100                        MOD-IDKVAINF-ENTER                                
059200                        MOD-IDKVAINF-NEXT                                 
059300     PERFORM MFS-RENSA-FAELT-IN                                           
059400     PERFORM MFS-RENSA-FAELT-UT                                           
059500     .                                                                    
059600     EJECT                                                                
059700 D-NAESTA-SIDA SECTION.                                                   
059800                                                                          
059900     IF MID-IDKR-NEXT > ZERO                                              
060000       MOVE MID-IDKR-NEXT     TO REQU-IDKR-START                          
060100     END-IF                                                               
060200                                                                          
060300     IF MID-IDKVAINF-NEXT > ZERO                                          
060400       MOVE MID-IDKVAINF-NEXT TO REQU-IDKVAINF-START                      
060500     END-IF                                                               
060510                                                                          
060600     PERFORM MFS-RENSA-FAELT-IN                                           
060700     PERFORM MFS-RENSA-FAELT-UT                                           
060800     .                                                                    
060900     EJECT                                                                
061000 E-SAMMA-SIDA SECTION.                                                    
061100                                                                          
061200     IF EGEN-MID OR HELP-MID                                              
061300       MOVE MID-IDKR-ENTER     TO REQU-IDKR-START                         
061400       MOVE MID-IDKVAINF-ENTER TO REQU-IDKVAINF-START                     
063400     ELSE                                                                 
063500       PERFORM MFS-RENSA-FAELT-IN                                         
063600     END-IF                                                               
063700     .                                                                    
063710 F-CALL-BIZ-LOGIC-W6013910 SECTION.                                       
063720                                                                          
063721     PERFORM FA-INIT-REQU                                                 
063722                                                                          
063730     CALL W6013910 USING REQU-AREA RESP-AREA                              
063740                         ALT-PCB ALT-IMS-PCB UPFA-PCB                     
063750                         INLA-PCB KVAH-PCB WDF5-PCB LEVA-PCB              
063760                         KVAE1-PCB ARTC-PCB                               
063761                         BENA-PCB INLC-PCB KVAE2-PCB WDB6-PCB             
063762                                                                          
063770     PERFORM FB-SET-MSG-AND-HILIGHT                                       
063771     IF RESP-FLAGGA-KR-HOPP = JA OR YES                                   
063772        CONTINUE                                                          
063773     ELSE                                                                 
063780       IF NOT WRONG-KEY                                                   
063790          PERFORM FC-MOVE-RESP-TO-MOD                                     
063791       END-IF                                                             
063792     END-IF                                                               
063796     .                                                                    
063800     EJECT                                                                
063900 FA-INIT-REQU SECTION.                                                    
064000                                                                          
064600     MOVE MID-FLAGGA-KR-HOPP        TO REQU-FLAGGA-KR-HOPP                
064700     MOVE MID-IDUSER-PRI            TO REQU-IDUSER-PRI                    
064800     MOVE MID-FLAGGA-PRI            TO REQU-FLAGGA-PRI                    
064900     MOVE MID-BEANST                TO REQU-BEANST                        
065000     MOVE MID-IDUSER-SEK            TO REQU-IDUSER-SEK                    
065100     MOVE MID-FLAGGA-SEK            TO REQU-FLAGGA-SEK                    
065110     MOVE MID-IDUSER-ADM            TO REQU-IDUSER-ADM                    
065200     MOVE MID-FLAGGA-ADM            TO REQU-FLAGGA-ADM                    
065300     MOVE MID-FLAGGA-GODK           TO REQU-FLAGGA-GODK                   
065310     MOVE MID-IDUSER-APR            TO REQU-IDUSER-APR                    
065400                                                                          
066700     MOVE '101'                     TO REQU-IDMSGVER                      
066800     MOVE MSGI-IDUSER               TO REQU-IDUSER                        
066900     MOVE MSGI-IDSPRAK              TO REQU-IDSPRAK                       
067000     .                                                                    
067100 FB-SET-MSG-AND-HILIGHT SECTION.                                          
067200                                                                          
067300     MOVE RESP-IDMSG-ERROR          TO MCNV-IDMSG-ERROR                   
067400                                       W-IDMSG-ERROR                      
067500     MOVE RESP-IDMSG-INFO           TO MCNV-IDMSG-INFO                    
067600     MOVE RESP-IDELMT-ERROR         TO MCNV-IDELMT-ERROR                  
067700     MOVE MSGI-IDSPRAK              TO MCNV-IDSPRAK                       
067800                                                                          
068900     CALL WL01MCNV USING MCNV-AREA                                        
069000                                                                          
069100     MOVE MCNV-MFSINF               TO MOD-TEMFSINF                       
069200     MOVE MCNV-MFSFEL               TO MOD-TEMFSFEL                       
069300     .                                                                    
069400     EJECT                                                                
069500 FC-MOVE-RESP-TO-MOD SECTION.                                             
069501                                                                          
069510     IF RESP-IDKR-START = ALL '+'                                         
069520       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDKR-ENTER                    
069530     ELSE                                                                 
069540       MOVE RESP-IDKR-START          TO MOD-IDKR-ENTER                    
069550     END-IF                                                               
069551                                                                          
069560     IF RESP-IDKR-NEXT = ALL '+'                                          
069570       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDKR-NEXT                     
069580     ELSE                                                                 
069590       MOVE RESP-IDKR-NEXT           TO MOD-IDKR-NEXT                     
069591     END-IF                                                               
069592                                                                          
069593     IF RESP-IDKVAINF-START = ALL '+'                                     
069594       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDKVAINF-ENTER                
069595     ELSE                                                                 
069596       MOVE RESP-IDKVAINF-START      TO MOD-IDKVAINF-ENTER                
069597     END-IF                                                               
069598                                                                          
069599     IF RESP-IDKVAINF-NEXT = ALL '+'                                      
069600       MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDKVAINF-NEXT                 
069610     ELSE                                                                 
069620       MOVE RESP-IDKVAINF-NEXT       TO MOD-IDKVAINF-NEXT                 
069630     END-IF                                                               
069631                                                                          
069640     IF RESP-IDARTNR = SPACE                                              
069650       MOVE MFS-ERASE-FIELD          TO MOD-IDARTNR                       
069660     ELSE                                                                 
069670       IF RESP-IDARTNR = ALL '+'                                          
069680         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR                       
069691       ELSE                                                               
069692         MOVE RESP-IDARTNR           TO MOD-IDARTNR                       
069694       END-IF                                                             
069700     END-IF                                                               
069701                                                                          
069710     IF RESP-BEART = SPACE                                                
069720       MOVE MFS-ERASE-FIELD          TO MOD-BEART                         
069730     ELSE                                                                 
069740       IF RESP-BEART = ALL '+'                                            
069750         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEART                         
069760       ELSE                                                               
069770         MOVE RESP-BEART             TO MOD-BEART                         
069780       END-IF                                                             
069790     END-IF                                                               
069791                                                                          
069792     IF RESP-BELEVART = SPACE                                             
069793       MOVE MFS-ERASE-FIELD          TO MOD-BELEVART                      
069794     ELSE                                                                 
069795       IF RESP-BELEVART = ALL '+'                                         
069796         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BELEVART                      
069797       ELSE                                                               
069798         MOVE RESP-BELEVART          TO MOD-BELEVART                      
069799       END-IF                                                             
069800     END-IF                                                               
069801                                                                          
069802     IF RESP-IDLEVNR = SPACE                                              
069803       MOVE MFS-ERASE-FIELD          TO MOD-IDLEVNR                       
069804     ELSE                                                                 
069805       IF RESP-IDLEVNR = ALL '+'                                          
069806         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDLEVNR                       
069807       ELSE                                                               
069808         MOVE RESP-IDLEVNR           TO MOD-IDLEVNR                       
069809       END-IF                                                             
069810     END-IF                                                               
069811                                                                          
069812     IF RESP-KVAVIS = SPACE                                               
069813       MOVE MFS-ERASE-FIELD          TO MOD-KVAVIS                        
069814     ELSE                                                                 
069815       IF RESP-KVAVIS = ALL '+'                                           
069816         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVAVIS                        
069817       ELSE                                                               
069818         MOVE RESP-KVAVIS            TO MOD-KVAVIS                        
069819       END-IF                                                             
069820     END-IF                                                               
069821                                                                          
069822     MOVE RESP-FLAGGA-KR-HOPP-ATTR   TO MOD-FLAGGA-KR-HOPP-ATTR           
069823     MOVE RESP-IDUSER-PRI-ATTR       TO MOD-IDUSER-PRI-ATTR               
069824     MOVE RESP-FLAGGA-PRI-ATTR       TO MOD-FLAGGA-PRI-ATTR               
069825     MOVE RESP-BEANST-ATTR           TO MOD-BEANST-ATTR                   
069826     MOVE RESP-IDUSER-SEK-ATTR       TO MOD-IDUSER-SEK-ATTR               
069827     MOVE RESP-FLAGGA-ADM-ATTR       TO MOD-FLAGGA-ADM-ATTR               
069828     MOVE RESP-FLAGGA-GODK-ATTR      TO MOD-FLAGGA-GODK-ATTR              
069829     MOVE RESP-IDUSER-APR-ATTR       TO MOD-IDUSER-APR-ATTR               
069830                                                                          
069831     IF RESP-FLAGGA-KR-HOPP = SPACE                                       
069832       MOVE MFS-ERASE-FIELD          TO MOD-FLAGGA-KR-HOPP                
069833     ELSE                                                                 
069834       IF RESP-FLAGGA-KR-HOPP = ALL '+'                                   
069835         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLAGGA-KR-HOPP                
069836       ELSE                                                               
069837         MOVE RESP-FLAGGA-KR-HOPP    TO MOD-FLAGGA-KR-HOPP                
069838       END-IF                                                             
069839     END-IF                                                               
069840                                                                          
069841     IF RESP-KVKVAPRIM = SPACE                                            
069842       MOVE MFS-ERASE-FIELD          TO MOD-KVKVAPRIM                     
069843     ELSE                                                                 
069844       IF RESP-KVKVAPRIM = ALL '+'                                        
069845         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVKVAPRIM                     
069846       ELSE                                                               
069847         MOVE RESP-KVKVAPRIM         TO MOD-KVKVAPRIM                     
069848       END-IF                                                             
069849     END-IF                                                               
069850                                                                          
069851     IF RESP-IDUSER-PRI = SPACE                                           
069852       MOVE MFS-ERASE-FIELD          TO MOD-IDUSER-PRI                    
069853     ELSE                                                                 
069854       IF RESP-IDUSER-PRI = ALL '+'                                       
069855         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDUSER-PRI                    
069856       ELSE                                                               
069857         MOVE RESP-IDUSER-PRI        TO MOD-IDUSER-PRI                    
069858       END-IF                                                             
069859     END-IF                                                               
069860                                                                          
069861     IF RESP-FLAGGA-PRI = SPACE                                           
069862       MOVE MFS-ERASE-FIELD          TO MOD-FLAGGA-PRI                    
069863     ELSE                                                                 
069864       IF RESP-FLAGGA-PRI = ALL '+'                                       
069865         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLAGGA-PRI                    
069866       ELSE                                                               
069867         MOVE RESP-FLAGGA-PRI        TO MOD-FLAGGA-PRI                    
069868       END-IF                                                             
069869     END-IF                                                               
069870                                                                          
069871     IF RESP-BEANST = SPACE                                               
069872       MOVE MFS-ERASE-FIELD          TO MOD-BEANST                        
069873     ELSE                                                                 
069874       IF RESP-BEANST = ALL '+'                                           
069875         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEANST                        
069876       ELSE                                                               
069877         MOVE RESP-BEANST            TO MOD-BEANST                        
069878       END-IF                                                             
069879     END-IF                                                               
069880                                                                          
069881     IF RESP-KVKVASEK = SPACE                                             
069882       MOVE MFS-ERASE-FIELD          TO MOD-KVKVASEK                      
069883     ELSE                                                                 
069884       IF RESP-KVKVASEK = ALL '+'                                         
069885         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVKVASEK                      
069886       ELSE                                                               
069887         MOVE RESP-KVKVASEK          TO MOD-KVKVASEK                      
069888       END-IF                                                             
069889     END-IF                                                               
069890                                                                          
069891     IF RESP-IDUSER-SEK = SPACE                                           
069892       MOVE MFS-ERASE-FIELD          TO MOD-IDUSER-SEK                    
069893     ELSE                                                                 
069894       IF RESP-IDUSER-SEK = ALL '+'                                       
069895         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDUSER-SEK                    
069896       ELSE                                                               
069897         MOVE RESP-IDUSER-SEK        TO MOD-IDUSER-SEK                    
069898       END-IF                                                             
069899     END-IF                                                               
069900                                                                          
069901     IF RESP-FLAGGA-SEK = SPACE                                           
069902       MOVE MFS-ERASE-FIELD          TO MOD-FLAGGA-SEK                    
069903     ELSE                                                                 
069904       IF RESP-FLAGGA-SEK = ALL '+'                                       
069905         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLAGGA-SEK                    
069906       ELSE                                                               
069907         MOVE RESP-FLAGGA-SEK        TO MOD-FLAGGA-SEK                    
069908       END-IF                                                             
069909     END-IF                                                               
069910                                                                          
069911     IF RESP-IDUSER-ADM = SPACE                                           
069912       MOVE MFS-ERASE-FIELD          TO MOD-IDUSER-ADM                    
069913     ELSE                                                                 
069914       IF RESP-IDUSER-ADM = ALL '+'                                       
069915         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDUSER-ADM                    
069916       ELSE                                                               
069917         MOVE RESP-IDUSER-ADM        TO MOD-IDUSER-ADM                    
069918       END-IF                                                             
069919     END-IF                                                               
069920                                                                          
069921     IF RESP-FLAGGA-ADM = SPACE                                           
069922       MOVE MFS-ERASE-FIELD          TO MOD-FLAGGA-ADM                    
069923     ELSE                                                                 
069924       IF RESP-FLAGGA-ADM = ALL '+'                                       
069925         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLAGGA-ADM                    
069926       ELSE                                                               
069927         MOVE RESP-FLAGGA-ADM        TO MOD-FLAGGA-ADM                    
069928       END-IF                                                             
069929     END-IF                                                               
069930                                                                          
069931     IF RESP-KDKVATYP = SPACE                                             
069932       MOVE MFS-ERASE-FIELD          TO MOD-KDKVATYP                      
069933     ELSE                                                                 
069934       IF RESP-KDKVATYP = ALL '+'                                         
069935         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDKVATYP                      
069936       ELSE                                                               
069937         MOVE RESP-KDKVATYP          TO MOD-KDKVATYP                      
069938       END-IF                                                             
069939     END-IF                                                               
069940                                                                          
069941     IF RESP-ADKVAULG = SPACE                                             
069942       MOVE MFS-ERASE-FIELD          TO MOD-ADKVAULG                      
069943     ELSE                                                                 
069944       IF RESP-ADKVAULG = ALL '+'                                         
069945         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ADKVAULG                      
069946       ELSE                                                               
069947         MOVE RESP-ADKVAULG          TO MOD-ADKVAULG                      
069948       END-IF                                                             
069949     END-IF                                                               
069950                                                                          
069951     IF RESP-UNDERLAG = SPACE                                             
069952       MOVE MFS-ERASE-FIELD          TO MOD-UNDERLAG                      
069953     ELSE                                                                 
069954       IF RESP-UNDERLAG = ALL '+'                                         
069955         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-UNDERLAG                      
069956       ELSE                                                               
069957         MOVE RESP-UNDERLAG          TO MOD-UNDERLAG                      
069958       END-IF                                                             
069959     END-IF                                                               
069960                                                                          
069961     IF RESP-SPEC-BEANST = SPACE                                          
069962       MOVE MFS-ERASE-FIELD          TO MOD-SPEC-BEANST                   
069963     ELSE                                                                 
069964       IF RESP-SPEC-BEANST = ALL '+'                                      
069965         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-SPEC-BEANST                   
069966       ELSE                                                               
069967         MOVE RESP-SPEC-BEANST       TO MOD-SPEC-BEANST                   
069968       END-IF                                                             
069969     END-IF                                                               
069970                                                                          
069971     IF RESP-KONTROLL = SPACE                                             
069972       MOVE MFS-ERASE-FIELD          TO MOD-KONTROLL                      
069973     ELSE                                                                 
069974       IF RESP-KONTROLL = ALL '+'                                         
069975         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KONTROLL                      
069976       ELSE                                                               
069977         MOVE RESP-KONTROLL          TO MOD-KONTROLL                      
069978       END-IF                                                             
069979     END-IF                                                               
069980                                                                          
069981     IF RESP-IDKR = SPACE                                                 
069982       MOVE MFS-ERASE-FIELD          TO MOD-IDKR                          
069983     ELSE                                                                 
069984       IF RESP-IDKR = ALL '+'                                             
069985         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDKR                          
069986       ELSE                                                               
069987         MOVE RESP-IDKR              TO MOD-IDKR                          
069988       END-IF                                                             
069989     END-IF                                                               
069990                                                                          
069991     IF RESP-FLAGGA-TEXT = SPACE                                          
069992       MOVE MFS-ERASE-FIELD          TO MOD-FLAGGA-TEXT                   
069993     ELSE                                                                 
069994       IF RESP-FLAGGA-TEXT = ALL '+'                                      
069995         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLAGGA-TEXT                   
069996       ELSE                                                               
069997         MOVE RESP-FLAGGA-TEXT       TO MOD-FLAGGA-TEXT                   
069998       END-IF                                                             
069999     END-IF                                                               
070000                                                                          
070001     IF RESP-FLAGGA-GODK = SPACE                                          
070002       MOVE MFS-ERASE-FIELD          TO MOD-FLAGGA-GODK                   
070003     ELSE                                                                 
070004       IF RESP-FLAGGA-GODK = ALL '+'                                      
070005         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLAGGA-GODK                   
070006       ELSE                                                               
070007         MOVE RESP-FLAGGA-GODK       TO MOD-FLAGGA-GODK                   
070008       END-IF                                                             
070009     END-IF                                                               
070010                                                                          
070011     IF RESP-IDTFN = SPACE                                                
070012       MOVE MFS-ERASE-FIELD          TO MOD-IDTFN                         
070013     ELSE                                                                 
070014       IF RESP-IDTFN = ALL '+'                                            
070015         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDTFN                         
070016       ELSE                                                               
070017         MOVE RESP-IDTFN             TO MOD-IDTFN                         
070018       END-IF                                                             
070019     END-IF                                                               
070020                                                                          
070021     MOVE +1                         TO INDX                              
070022     PERFORM UNTIL INDX > 4                                               
070023       IF RESP-TEKRFEL(INDX) = SPACE                                      
070024         MOVE MFS-ERASE-FIELD          TO MOD-TEKRFEL(INDX)               
070025       ELSE                                                               
070026         IF RESP-TEKRFEL(INDX) = ALL '+'                                  
070027           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEKRFEL(INDX)               
070028         ELSE                                                             
070029           MOVE RESP-TEKRFEL(INDX)     TO MOD-TEKRFEL(INDX)               
070030         END-IF                                                           
070031       END-IF                                                             
070032       ADD +1                          TO INDX                            
070033     END-PERFORM                                                          
070034                                                                          
070035     IF RESP-EMPLID-TEXT = SPACE                                          
070036       MOVE MFS-ERASE-FIELD          TO MOD-EMPLID-TEXT                   
070037     ELSE                                                                 
070038       IF RESP-EMPLID-TEXT = ALL '+'                                      
070039         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-EMPLID-TEXT                   
070040       ELSE                                                               
070041         MOVE RESP-EMPLID-TEXT       TO MOD-EMPLID-TEXT                   
070042       END-IF                                                             
070043     END-IF                                                               
070044                                                                          
070045     IF RESP-IDUSER-APR = SPACE                                           
070046       MOVE MFS-ERASE-FIELD          TO MOD-IDUSER-APR                    
070047     ELSE                                                                 
070048       IF RESP-IDUSER-APR = ALL '+'                                       
070049         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDUSER-APR                    
070050       ELSE                                                               
070051         MOVE RESP-IDUSER-APR        TO MOD-IDUSER-APR                    
070052       END-IF                                                             
070053     END-IF                                                               
070054                                                                          
070060     .                                                                    
070100                                                                          
208900 MFS-RENSA-FAELT-IN SECTION.                                              
209000                                                                          
209100*    --- ALLA INDATA-FÄLT                                                 
209200     MOVE MFS-RENSA-FAELT TO MOD-FLAGGA-KR-HOPP                           
209300                             MOD-IDUSER-PRI                               
209400                             MOD-IDUSER-ADM                               
209500                             MOD-BEANST                                   
209600                             MOD-IDUSER-SEK                               
209700                             MOD-FLAGGA-PRI                               
209800                             MOD-FLAGGA-ADM                               
209900                             MOD-FLAGGA-SEK                               
210000                             MOD-FLAGGA-GODK                              
210010                             MOD-IDUSER-APR                               
210100     .                                                                    
210200     SKIP2                                                                
210300 MFS-RENSA-FAELT-UT SECTION.                                              
210400                                                                          
210500*    --- ALLA UTDATA-FÄLT                                                 
210600     MOVE MFS-RENSA-FAELT TO MOD-IDKR-ENTER                               
210700                             MOD-IDKR-NEXT                                
210800                             MOD-IDKVAINF-ENTER                           
210900                             MOD-IDKVAINF-NEXT                            
211000                             MOD-IDARTNR                                  
211100                             MOD-BEART                                    
211200                             MOD-BELEVART                                 
211300                             MOD-IDLEVNR                                  
211400                             MOD-KVAVIS                                   
211500                             MOD-KVKVAPRIM                                
211600                             MOD-KVKVASEK                                 
211700                             MOD-KDKVATYP                                 
211800                             MOD-IDKR                                     
211900                             MOD-UNDERLAG                                 
212000                             MOD-SPEC-BEANST                              
212100                             MOD-ADKVAULG                                 
212200                             MOD-KONTROLL                                 
212300                             MOD-IDTFN                                    
212400                             MOD-TEKRFEL (1)                              
212500                             MOD-TEKRFEL (2)                              
212600                             MOD-TEKRFEL (3)                              
212700                             MOD-TEKRFEL (4)                              
212710                             MOD-EMPLID-TEXT                              
212800     .                                                                    
212900     SKIP2                                                                
223400* --- IMS SEKTIONER ---                                                   
223500     SKIP3                                                                
223600 IMS-GET-MSG SECTION.                                                     
223700                                                                          
223800     MOVE '  QC' TO GODK-STATUSKODER                                      
223900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
224000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
224100     PERFORM IMS-STATUSKONTROLL                                           
224200     .                                                                    
224300     SKIP3                                                                
224400 IMS-INSERT-MSG SECTION.                                                  
224500                                                                          
224600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
224700       MOVE '0' TO MFS-KDHUVOMR                                           
224800     END-IF                                                               
224900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
225000     MOVE SPACE TO GODK-STATUSKODER                                       
225100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
225200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
225300     PERFORM IMS-STATUSKONTROLL                                           
225400     .                                                                    
225500     SKIP3                                                                
256000 IMS-STATUSKONTROLL SECTION.                                              
256100                                                                          
256200     SET STATUS-IX TO 1                                                   
256300     SEARCH GODK-STATUS                                                   
256400       AT END                                                             
256500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
256600         DELIMITED BY SIZE INTO FELTEXT                                   
256700         CALL FELLOG                                                      
256800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
256900         CONTINUE                                                         
257000     END-SEARCH                                                           
257100     .                                                                    
