000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF029300.                                                
000400 AUTHOR.         SARASWATHY S.                                            
000500 DATE-WRITTEN.   20/04/06.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000801*    NAME:                                                                
000810*        'CARPARTS.BILLIT.APPROVEDYRCURRENCIESLOCATE'                     
000900*    FUNCTION:                                                            
001000*        CURRENCIES YEARLY SCREEN - LOCATE                                
001100*        READS NEW DB2 TABLE FOR THE YEARLY CURRENCIES T01CUYE            
001110*        ANSWER VIA SUBPROGRAM WZ01SUB.                                   
001120*                                                                         
001130*        THE PROGRAM READS     TABLE T01LSEL                              
001140*        THE PROGRAM READS     TABLE T01CUYE                              
001150*                                                                         
001151*    INDATA.                                                              
001152*        TRANSACTION: WF0293T                                             
001153*        REQUEST:     WF0293I1                                            
001154*                                                                         
001155*    OUTDATA.                                                             
001156*        RESPONSE:    WF0293O1                                            
001500*                                                                         
002110     SKIP3                                                                
002120 ENVIRONMENT DIVISION.                                                    
002130 INPUT-OUTPUT SECTION.                                                    
002140 FILE-CONTROL.                                                            
002150 DATA DIVISION.                                                           
002160 FILE SECTION.                                                            
002170     EJECT                                                                
002180 WORKING-STORAGE SECTION.                                                 
002190 77  IDPGM                       PIC X(08)   VALUE 'WF029300'.            
002191                                                                          
002192*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND. ***           
002193 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002194 77  KDRC-DISPLAY                PIC Z(5).                                
002195                                                                          
002196 77  YES                         PIC X       VALUE 'Y'.                   
002197 77  NOO                         PIC X       VALUE 'N'.                   
002198                                                                          
002199 77  WS-MAX-LINES                PIC S9(3)  VALUE +500    COMP-3.         
002200 77  WS-ACTIVE                   PIC X(8)   VALUE '00000000'.             
002201                                                                          
002202 77  KEYS-SW                     PIC X       VALUE SPACE.                 
002203     88  KEYS-OK                             VALUE 'Y'.                   
002204     88  KEYS-WRONG                          VALUE 'N'.                   
002205     EJECT                                                                
002206                                                                          
002207*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
002208 01  GENERAL-SUBPROGRAMS.                                                 
002209     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
002210     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
002211     SKIP3                                                                
002212                                                                          
002213*    --- PARAMETRARS TO ABEND                                             
002214 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
002215 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
002216 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
002217 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
002218     SKIP3                                                                
002219                                                                          
002220 01  MESSAGE-CODES.                                                       
002221     03  ERROR-CODES.                                                     
002222       05 ERR-WRONG-KEY              PIC X(3)    VALUE '022'.             
002223       05 IS-INVALID                 PIC X(3)    VALUE '023'.             
002224       05 MUST-BE-NUMERIC            PIC X(3)    VALUE '024'.             
002225       05 NOT-FOUND                  PIC X(3)    VALUE '025'.             
002226       05 MUST-BE-ENTERED            PIC X(3)    VALUE '026'.             
002227       05 LINE-NOT-FOUND             PIC X(3)    VALUE '027'.             
002228       05 MORE-LINE-EXIST            PIC X(3)    VALUE '028'.             
002229       05 SYSTEM-ERROR               PIC X(3)    VALUE '099'.             
002230     EJECT                                                                
002231*                                                                         
002232 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
002233     SKIP3                                                                
002234 01  -COPY WZ01SUB                                                        
002235     EJECT                                                                
002236                                                                          
002237 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
002238     SKIP3                                                                
002239 01  REQU-AREA.                                                           
002240*    03  -COPY WZ01REQU                                                   
002241*    03  -COPY WF0293I1                                                   
002242     EJECT                                                                
002243                                                                          
002244 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
002245     SKIP3                                                                
002246 01  RESP-AREA.                                                           
002247*    03  -COPY WZ01RESP                                                   
002248*    03  -COPY WF0293O1                                                   
002249     EJECT                                                                
002250                                                                          
002251 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
002252       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
002253                                                                          
002254 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
002255 01  DB2-WS.                                                              
002256     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
002257         88  CURSOR-OK                       VALUE 000.                   
002258         88  LINES-FOUND                     VALUE 000.                   
002259         88  LINES-MISSING                   VALUE 100.                   
002260         88  RESCORCE-WRONG                  VALUE 904.                   
002261                                                                          
002262     03  GOOD-SQLCODECODES.                                               
002263         05  GOOD-SQLCODE OCCURS 5                                        
002264             INDEXED BY SQLCODE-IX PIC 9(3).                              
002265     EJECT                                                                
002266                                                                          
002267 01  WS-IX                      PIC S9(9)    VALUE ZERO BINARY.           
002268 01  WS-COUNTER-T01CUYE         PIC S9(7)    COMP-3 VALUE ZERO.           
002269                                                                          
002270 01  WS-AREA.                                                             
002271     03 WS-KDSTATUS             PIC S9(3)    COMP-3 VALUE ZERO.           
002272     03 WS-IDLEGSEL             PIC X(4)     VALUE SPACE.                 
002273     03 WS-BELEGRAD-1           PIC X(35)    VALUE SPACE.                 
002274     03 WS-KVRADER              PIC Z(4)9(1) VALUE ZERO.                  
002275     03 WS-KDVALISO-LINE        PIC X(3)     VALUE SPACE.                 
002276     03 WS-DASTADAT-LINE        PIC X(8)     VALUE SPACE.                 
002277     03 WS-PRKURS-LINE          PIC S9(6)V9(6) COMP-3 VALUE ZERO.         
002278     03 WS-PRKURS-LINE-Z        PIC Z(5)9.9(6) VALUE ZERO.                
002279     03 WS-REVALUTA-LINE-FR     PIC S9(5)    COMP-3 VALUE ZERO.           
002280     03 WS-REVALUTA-LINE-FR-Z   PIC Z(4)9    VALUE ZERO.                  
002281     03 WS-REVALUTA-LINE-TO     PIC S9(5)    COMP-3 VALUE ZERO.           
002282     03 WS-REVALUTA-LINE-TO-Z   PIC Z(4)9    VALUE ZERO.                  
002283     03 WS-DAREGDAT-LINE        PIC X(8)     VALUE SPACE.                 
002284     03 WS-IDMSG-INFO           PIC X(3)     VALUE SPACE.                 
002285     03 WS-IDMSG-ERROR          PIC X(3)     VALUE SPACE.                 
002286     03 WS-IDELMT-ERROR         PIC X(16)    VALUE SPACE.                 
002287     EJECT                                                                
002288                                                                          
002289 01  WS-DASTADAT-START          PIC X(8)     VALUE SPACE.                 
002290                                                                          
002291 01  FILLER                    PIC X(16)    VALUE 'T01LSEL-AREA'.         
002292*01  -COPY T01LSEL -PRE T01LSEL-                                          
002293     EJECT                                                                
002294                                                                          
002295 01  FILLER                    PIC X(16)    VALUE 'T01CUYE-AREA'.         
002296*01  -COPY T01CUYE -PRE T01CUYE-                                          
002297     EJECT                                                                
002298                                                                          
002299     EXEC SQL INCLUDE T01LSEL END-EXEC.                                   
002300     EJECT                                                                
002301     EXEC SQL INCLUDE T01CUYE END-EXEC.                                   
002302     EJECT                                                                
002303                                                                          
002304 LINKAGE SECTION.                                                         
002305 PROCEDURE DIVISION.                                                      
002306 MAIN SECTION.                                                            
002307                                                                          
002308     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
002309     IF SUB-KDRC = ZERO                                                   
002310       PERFORM A-INIT                                                     
002311       PERFORM B-CHECK-KEYS                                               
002312       IF KEYS-OK                                                         
002313         PERFORM C-CHECK-INDATA                                           
002314       END-IF                                                             
002315       IF KEYS-OK                                                         
002316         PERFORM D-PERFORM-REQUEST                                        
002317       END-IF                                                             
002318       PERFORM E-READ-SHOW-INFO                                           
002319       PERFORM S02-RETURN-RESPONSE                                        
002320     END-IF                                                               
002321                                                                          
002322     MOVE ZERO TO RETURN-CODE                                             
002323     GOBACK                                                               
002324     .                                                                    
002325     EJECT                                                                
002326                                                                          
002327 A-INIT SECTION.                                                          
002328                                                                          
002329     MOVE YES                   TO KEYS-SW                                
002330                                                                          
002331     MOVE ALL '+'               TO RESP-AREA                              
002332     MOVE SPACE TO RESP-IDMSG-ERROR                                       
002333     MOVE SPACE TO RESP-IDMSG-INFO                                        
002334     MOVE SPACE TO RESP-IDELMT-ERROR                                      
002335                                                                          
002336     MOVE ZERO                  TO WS-COUNTER-T01CUYE                     
002337                                   RESP-KVRADER                           
002338                                                                          
002339     INITIALIZE GOOD-SQLCODECODES                                         
002340     .                                                                    
002341     EJECT                                                                
002342                                                                          
002343 B-CHECK-KEYS SECTION.                                                    
002346     IF REQU-KDPGMACT = 'S'                                               
002347     AND REQU-IDMSGVER NUMERIC                                            
002348       CONTINUE                                                           
002349     ELSE                                                                 
002350       MOVE NOO TO KEYS-SW                                                
002351     END-IF                                                               
002352                                                                          
002353     IF REQU-IDLEGSEL-KEY = SPACE OR = ALL '+'                            
002354       MOVE NOO TO KEYS-SW                                                
002355     END-IF                                                               
002356                                                                          
002357     IF REQU-IDUSER = SPACE OR = ALL '+'                                  
002358       MOVE NOO TO KEYS-SW                                                
002359     END-IF                                                               
002360                                                                          
002361     IF KEYS-WRONG                                                        
002362       MOVE ERR-WRONG-KEY TO WS-IDMSG-ERROR                               
002363       IF REQU-KDPGMACT = 'S'                                             
002364         CONTINUE                                                         
002365       ELSE                                                               
002366         MOVE SYSTEM-ERROR TO WS-IDMSG-ERROR                              
002367         MOVE 'KDPGMACT'   TO WS-IDELMT-ERROR                             
002368       END-IF                                                             
002369       IF REQU-IDMSGVER NUMERIC                                           
002370         CONTINUE                                                         
002371       ELSE                                                               
002372         MOVE SYSTEM-ERROR    TO WS-IDMSG-ERROR                           
002373         MOVE 'IDMSGVER'      TO WS-IDELMT-ERROR                          
002374       END-IF                                                             
002375       IF REQU-IDUSER = SPACE OR = ALL '+'                                
002376         MOVE SYSTEM-ERROR    TO WS-IDMSG-ERROR                           
002377         MOVE 'IDUSER'        TO WS-IDELMT-ERROR                          
002378       ELSE                                                               
002379         CONTINUE                                                         
002380       END-IF                                                             
002381     END-IF                                                               
002382     .                                                                    
002383     EJECT                                                                
002384                                                                          
002385 C-CHECK-INDATA SECTION.                                                  
002386     IF REQU-KDPGMACT = 'S'                                               
002387       PERFORM DB2-SELECT-T01LSEL                                         
002388       IF LINES-FOUND                                                     
002389         CONTINUE                                                         
002390       ELSE                                                               
002391         MOVE NOT-FOUND  TO WS-IDMSG-ERROR                                
002392         MOVE 'IDLEGSEL' TO WS-IDELMT-ERROR                               
002393         MOVE NOO TO KEYS-SW                                              
002394       END-IF                                                             
002395     END-IF                                                               
002396     IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                            
002397       PERFORM DB2-OPEN-T01CUYE-CRS                                       
002400       PERFORM DB2-FETCH-T01CUYE-CRS                                      
002408       IF LINES-FOUND                                                     
002409         MOVE WS-DASTADAT-LINE TO WS-DASTADAT-START                       
002410       ELSE                                                               
002411         MOVE LINE-NOT-FOUND TO WS-IDMSG-ERROR                            
002412         MOVE NOO TO KEYS-SW                                              
002413       END-IF                                                             
002414       PERFORM DB2-CLOSE-T01CUYE-CRS                                      
002415     ELSE                                                                 
002416       PERFORM DB2-OPEN-T01CUYE-CRS-2                                     
002417       PERFORM DB2-FETCH-T01CUYE-CRS-2                                    
002418       IF LINES-FOUND                                                     
002419         CONTINUE                                                         
002420       ELSE                                                               
002421         MOVE LINE-NOT-FOUND TO WS-IDMSG-ERROR                            
002422         MOVE NOO TO KEYS-SW                                              
002423       END-IF                                                             
002424       PERFORM DB2-CLOSE-T01CUYE-CRS-2                                    
002425     END-IF                                                               
002426     .                                                                    
002427     EJECT                                                                
002428                                                                          
002429 D-PERFORM-REQUEST SECTION.                                               
002430     IF REQU-KDPGMACT = 'S'                                               
002431       IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                          
002432         PERFORM DB2-COUNT-CRS-1                                          
002436       ELSE                                                               
002437         PERFORM DB2-COUNT-CRS-2                                          
002438       END-IF                                                             
002439       IF WS-COUNTER-T01CUYE NOT = ZERO                                   
002440         IF WS-COUNTER-T01CUYE > WS-MAX-LINES                             
002441            MOVE MORE-LINE-EXIST TO WS-IDMSG-ERROR                        
002442            MOVE NOO TO KEYS-SW                                           
002443         END-IF                                                           
002444       ELSE                                                               
002445         MOVE NOO TO KEYS-SW                                              
002446         MOVE LINE-NOT-FOUND TO WS-IDMSG-ERROR                            
002447       END-IF                                                             
002448       IF KEYS-OK                                                         
002449         IF REQU-KDVALISO-KEY = SPACE OR = ALL '+'                        
002450           PERFORM DB2-OPEN-T01CUYE-CRS-3                                 
002451           PERFORM DB2-FETCH-T01CUYE-CRS-3                                
002452           MOVE ZERO TO WS-IX                                             
002453           PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES            
002460             PERFORM F-READ-SHOW-INFO-TABELL                              
002461             PERFORM DB2-FETCH-T01CUYE-CRS-3                              
002462           END-PERFORM                                                    
002463           PERFORM DB2-CLOSE-T01CUYE-CRS-3                                
002464         ELSE                                                             
002465           PERFORM DB2-OPEN-T01CUYE-CRS-2                                 
002466           PERFORM DB2-FETCH-T01CUYE-CRS-2                                
002467           MOVE ZERO TO WS-IX                                             
002468           PERFORM UNTIL LINES-MISSING OR WS-IX = WS-MAX-LINES            
002469             PERFORM F-READ-SHOW-INFO-TABELL                              
002470             PERFORM DB2-FETCH-T01CUYE-CRS-2                              
002471           END-PERFORM                                                    
002472           PERFORM DB2-CLOSE-T01CUYE-CRS-2                                
002473         END-IF                                                           
002474       END-IF                                                             
002475     END-IF                                                               
002476     .                                                                    
002477     EJECT                                                                
002478                                                                          
002479 E-READ-SHOW-INFO SECTION.                                                
002480     IF REQU-KDPGMACT = 'S'                                               
002481       MOVE REQU-IDMSGVER            TO RESP-IDMSGVER                     
002482       MOVE WS-IDMSG-INFO            TO RESP-IDMSG-INFO                   
002483       MOVE WS-IDMSG-ERROR           TO RESP-IDMSG-ERROR                  
002484       MOVE WS-IDELMT-ERROR          TO RESP-IDELMT-ERROR                 
002485       MOVE REQU-IDLEGSEL-KEY        TO RESP-IDLEGSEL-KEY                 
002486       MOVE REQU-KDVALISO-KEY        TO RESP-KDVALISO-KEY                 
002487       MOVE WS-BELEGRAD-1            TO RESP-BELEGRAD-1                   
002488       MOVE WS-IX                    TO RESP-KVRADER                      
002489     END-IF                                                               
002490     .                                                                    
002491     EJECT                                                                
002492                                                                          
002493 F-READ-SHOW-INFO-TABELL SECTION.                                         
002494     IF REQU-KDPGMACT = 'S'                                               
002495      ADD 1 TO WS-IX                                                      
002496      MOVE WS-KDVALISO-LINE      TO RESP-KDVALISO-LINE(WS-IX)             
002497      MOVE WS-DASTADAT-LINE      TO RESP-DASTADAT-LINE(WS-IX)             
002498      MOVE WS-PRKURS-LINE        TO WS-PRKURS-LINE-Z                      
002499      MOVE WS-PRKURS-LINE-Z      TO RESP-PRKURS-LINE(WS-IX)               
002500      MOVE WS-REVALUTA-LINE-FR   TO WS-REVALUTA-LINE-FR-Z                 
002501      MOVE WS-REVALUTA-LINE-FR-Z TO RESP-REVALUTA-FROM-LINE(WS-IX)        
002502      MOVE WS-REVALUTA-LINE-TO   TO WS-REVALUTA-LINE-TO-Z                 
002503      MOVE WS-REVALUTA-LINE-TO-Z TO RESP-REVALUTA-TO-LINE(WS-IX)          
002504      MOVE WS-DAREGDAT-LINE      TO RESP-DAREGDAT-LINE(WS-IX)             
002505     END-IF                                                               
002506     .                                                                    
002507     EJECT                                                                
002508                                                                          
002509*    --- DISPATCHER-SECTION START                                         
002510 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
002511     MOVE 'GETARG'                               TO SUB-KDFUNC            
002512     MOVE 'CARPARTS.BILLIT.APPROVEDYRCURRENCIESLOCATE'                    
002513       TO SUB-ADDISPABS                                                   
002514     MOVE LENGTH OF REQU-AREA                    TO SUB-KVDLEN            
002515                                                                          
002516     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
002517                                                                          
002518     IF SUB-KDRC > 0                                                      
002519       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
002520       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
002521       DELIMITED BY SIZE INTO ERROR-TEXT                                  
002522       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
002523     END-IF                                                               
002524     .                                                                    
002525     SKIP3                                                                
002526                                                                          
002527 S02-RETURN-RESPONSE SECTION.                                             
002528     MOVE 'RETURN'                   TO SUB-KDFUNC                        
002529                                                                          
002530     COMPUTE SUB-KVDLEN       = LENGTH OF RESP-AREA                       
002531                              - ((WS-MAX-LINES - WS-IX)                   
002532                              * LENGTH OF RESP-TABELLRAD)                 
002533                                                                          
002534     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
002535                                                                          
002536     IF SUB-KDRC > 0                                                      
002537       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
002538       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
002539       DELIMITED BY SIZE INTO ERROR-TEXT                                  
002540       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
002541     END-IF                                                               
002542     .                                                                    
002543     EJECT                                                                
002544                                                                          
002545 DB2-SELECT-T01LSEL SECTION.                                              
002546     MOVE 000100  TO GOOD-SQLCODECODES                                    
002547     EXEC SQL                                                             
002548         SELECT  IDLEGSEL, KDSTATUS, BELEGRAD_1                           
002549                                                                          
002550         INTO :WS-IDLEGSEL, :WS-KDSTATUS, :WS-BELEGRAD-1                  
002551                                                                          
002552         FROM    T01LSEL                                                  
002553                                                                          
002554         WHERE   IDLEGSEL = :REQU-IDLEGSEL-KEY                            
002555         AND     KDSTATUS = 001                                           
002556     END-EXEC                                                             
002557     MOVE SQLCODE TO SQLCODE-WS                                           
002558     PERFORM DB2-STATUS-CHECK                                             
002559     .                                                                    
002560     EJECT                                                                
002561                                                                          
002562 DB2-COUNT-CRS-1 SECTION.                                                 
002563     MOVE 000100  TO GOOD-SQLCODECODES                                    
002564     EXEC SQL                                                             
002565                                                                          
002566           SELECT COUNT(*)                                                
002567                                                                          
002568           INTO  :WS-COUNTER-T01CUYE                                      
002569                                                                          
002570           FROM   T01CUYE                                                 
002571                                                                          
002572           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
002574                                                                          
002575     END-EXEC                                                             
002576                                                                          
002577     MOVE SQLCODE TO SQLCODE-WS                                           
002578     PERFORM DB2-STATUS-CHECK                                             
002579     .                                                                    
002580     EJECT                                                                
002581                                                                          
002582 DB2-COUNT-CRS-2 SECTION.                                                 
002583     MOVE 000100  TO GOOD-SQLCODECODES                                    
002584     EXEC SQL                                                             
002585                                                                          
002586           SELECT COUNT(*)                                                
002587                                                                          
002588           INTO  :WS-COUNTER-T01CUYE                                      
002589                                                                          
002590           FROM   T01CUYE                                                 
002591                                                                          
002592           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
002593           AND      KDVALISO = :REQU-KDVALISO-KEY                         
002594                                                                          
002595     END-EXEC                                                             
002596     MOVE SQLCODE TO SQLCODE-WS                                           
002597     PERFORM DB2-STATUS-CHECK                                             
002598     .                                                                    
002599     EJECT                                                                
002600                                                                          
002601 DB2-OPEN-T01CUYE-CRS SECTION.                                            
002602     MOVE 000100 TO GOOD-SQLCODECODES                                     
002603     EXEC SQL                                                             
002604         DECLARE T01CUYE-CRS CURSOR WITH HOLD FOR                         
002605                                                                          
002606           SELECT  IDLEGSEL, KDVALISO,                                    
002607                   DASTADAT, REVALUTA_FROM, REVALUTA_TO,                  
002608                   PRKURS_NEW, DAREGDAT                                   
002609                                                                          
002610           FROM    T01CUYE                                                
002611                                                                          
002612           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
002613           AND      DADELDAT = :WS-ACTIVE                                 
002614                                                                          
002615           ORDER BY IDLEGSEL                                              
002616                  , KDVALISO                                              
002617                  , DASTADAT DESC                                         
002618     END-EXEC                                                             
002619     MOVE 000100  TO GOOD-SQLCODECODES                                    
002620                                                                          
002621     EXEC SQL                                                             
002622        OPEN T01CUYE-CRS                                                  
002623     END-EXEC                                                             
002624     MOVE SQLCODE TO SQLCODE-WS                                           
002625     PERFORM DB2-STATUS-CHECK                                             
002626     .                                                                    
002627     EJECT                                                                
002628                                                                          
002629 DB2-FETCH-T01CUYE-CRS SECTION.                                           
002630     MOVE 000100  TO GOOD-SQLCODECODES                                    
002631     EXEC SQL                                                             
002632                                                                          
002633         FETCH T01CUYE-CRS                                                
002634                                                                          
002635         INTO :WS-IDLEGSEL,                                               
002636              :WS-KDVALISO-LINE,                                          
002637              :WS-DASTADAT-LINE,                                          
002638              :WS-REVALUTA-LINE-FR,                                       
002639              :WS-REVALUTA-LINE-TO,                                       
002640              :WS-PRKURS-LINE,                                            
002641              :WS-DAREGDAT-LINE                                           
002642     END-EXEC                                                             
002643     MOVE SQLCODE TO SQLCODE-WS                                           
002644     PERFORM DB2-STATUS-CHECK                                             
002645     .                                                                    
002646     EJECT                                                                
002647                                                                          
002648 DB2-CLOSE-T01CUYE-CRS SECTION.                                           
002649                                                                          
002650     EXEC SQL                                                             
002651        CLOSE T01CUYE-CRS                                                 
002652     END-EXEC                                                             
002653     .                                                                    
002654     EJECT                                                                
002655                                                                          
002656 DB2-OPEN-T01CUYE-CRS-2 SECTION.                                          
002657     MOVE 000100 TO GOOD-SQLCODECODES                                     
002658     EXEC SQL                                                             
002659         DECLARE T01CUYE-CRS-2 CURSOR WITH HOLD FOR                       
002660                                                                          
002661           SELECT  IDLEGSEL, KDVALISO,                                    
002662                   DASTADAT, REVALUTA_FROM, REVALUTA_TO,                  
002663                   PRKURS_NEW, DAREGDAT                                   
002664                                                                          
002665           FROM    T01CUYE                                                
002666                                                                          
002667           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
002668           AND      KDVALISO = :REQU-KDVALISO-KEY                         
002669           AND      DADELDAT = :WS-ACTIVE                                 
002670                                                                          
002671           ORDER BY IDLEGSEL                                              
002672                  , KDVALISO                                              
002673                  , DASTADAT DESC                                         
002674     END-EXEC                                                             
002675     MOVE 000100  TO GOOD-SQLCODECODES                                    
002676                                                                          
002677     EXEC SQL                                                             
002678        OPEN T01CUYE-CRS-2                                                
002679     END-EXEC                                                             
002680     MOVE SQLCODE TO SQLCODE-WS                                           
002681     PERFORM DB2-STATUS-CHECK                                             
002682     .                                                                    
002683     EJECT                                                                
002684                                                                          
002685 DB2-FETCH-T01CUYE-CRS-2 SECTION.                                         
002686     MOVE 000100  TO GOOD-SQLCODECODES                                    
002687     EXEC SQL                                                             
002688                                                                          
002689         FETCH T01CUYE-CRS-2                                              
002690                                                                          
002691         INTO :WS-IDLEGSEL,                                               
002692              :WS-KDVALISO-LINE,                                          
002693              :WS-DASTADAT-LINE,                                          
002694              :WS-REVALUTA-LINE-FR,                                       
002695              :WS-REVALUTA-LINE-TO,                                       
002696              :WS-PRKURS-LINE,                                            
002697              :WS-DAREGDAT-LINE                                           
002698     END-EXEC                                                             
002699     MOVE SQLCODE TO SQLCODE-WS                                           
002700     PERFORM DB2-STATUS-CHECK                                             
002701     .                                                                    
002702     EJECT                                                                
002703                                                                          
002704 DB2-CLOSE-T01CUYE-CRS-2 SECTION.                                         
002705                                                                          
002706     EXEC SQL                                                             
002707        CLOSE T01CUYE-CRS-2                                               
002708     END-EXEC                                                             
002709     .                                                                    
002710     EJECT                                                                
002711                                                                          
002712 DB2-OPEN-T01CUYE-CRS-3 SECTION.                                          
002713     MOVE 000100 TO GOOD-SQLCODECODES                                     
002714     EXEC SQL                                                             
002715         DECLARE T01CUYE-CRS-3 CURSOR WITH HOLD FOR                       
002716                                                                          
002717           SELECT  IDLEGSEL, KDVALISO,                                    
002718                   DASTADAT, REVALUTA_FROM, REVALUTA_TO,                  
002719                   PRKURS_NEW, DAREGDAT                                   
002720                                                                          
002721           FROM    T01CUYE                                                
002722                                                                          
002723           WHERE    IDLEGSEL = :REQU-IDLEGSEL-KEY                         
002725           AND      DADELDAT = :WS-ACTIVE                                 
002726                                                                          
002727           ORDER BY IDLEGSEL                                              
002728                  , KDVALISO                                              
002729                  , DASTADAT DESC                                         
002730     END-EXEC                                                             
002731     MOVE 000100  TO GOOD-SQLCODECODES                                    
002732                                                                          
002733     EXEC SQL                                                             
002734        OPEN T01CUYE-CRS-3                                                
002735     END-EXEC                                                             
002736     MOVE SQLCODE TO SQLCODE-WS                                           
002737     PERFORM DB2-STATUS-CHECK                                             
002738     .                                                                    
002739     EJECT                                                                
002740                                                                          
002741 DB2-FETCH-T01CUYE-CRS-3 SECTION.                                         
002742     MOVE 000100  TO GOOD-SQLCODECODES                                    
002743     EXEC SQL                                                             
002744                                                                          
002745         FETCH T01CUYE-CRS-3                                              
002746                                                                          
002747         INTO :WS-IDLEGSEL,                                               
002748              :WS-KDVALISO-LINE,                                          
002749              :WS-DASTADAT-LINE,                                          
002750              :WS-REVALUTA-LINE-FR,                                       
002751              :WS-REVALUTA-LINE-TO,                                       
002752              :WS-PRKURS-LINE,                                            
002753              :WS-DAREGDAT-LINE                                           
002754     END-EXEC                                                             
002755     MOVE SQLCODE TO SQLCODE-WS                                           
002756     PERFORM DB2-STATUS-CHECK                                             
002757     .                                                                    
002758     EJECT                                                                
002759                                                                          
002760 DB2-CLOSE-T01CUYE-CRS-3 SECTION.                                         
002761                                                                          
002762     EXEC SQL                                                             
002763        CLOSE T01CUYE-CRS-3                                               
002764     END-EXEC                                                             
002765     .                                                                    
002766     EJECT                                                                
002767                                                                          
002768 DB2-STATUS-CHECK     SECTION.                                            
002769     SET SQLCODE-IX TO 1                                                  
002770     SEARCH GOOD-SQLCODE                                                  
002771       AT END                                                             
002772          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
002773          DELIMITED BY SIZE INTO ERROR-TEXT                               
002774          CALL ABEND USING RKOD-ABEND-DB2                                 
002775       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
002776     END-SEARCH                                                           
002780     .                                                                    
