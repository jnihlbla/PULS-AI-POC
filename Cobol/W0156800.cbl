       ID DIVISION.                                                             
       PROGRAM-ID. W0156800.                                                    
       AUTHOR.   REXX.                                                          
       DATE-WRITTEN.   04-07-27.                                                
       DATE-COMPILED.                                                           
                                                                                
                                                                                
      *   FUNKTION:                                                             
      *       PROGRAMMET UPPDATERAR WDE401                                      
      *                                                                         
      *                                                                         
      *                                                                         
      *                                                                         
                                                                                
            SKIP3                                                               
        ENVIRONMENT DIVISION.                                                   
            SKIP2                                                               
        INPUT-OUTPUT SECTION.                                                   
                                                                                
        FILE-CONTROL.                                                           
            SELECT INDATA           ASSIGN TO      W01565D1.                    
            SKIP2                                                               
      *                                                                         
            EJECT                                                               
        DATA DIVISION.                                                          
            SKIP3                                                               
        FILE SECTION.                                                           
            SKIP3                                                               
        FD INDATA                                                               
            LABEL RECORD STANDARD                                               
            RECORDING F                                                         
            BLOCK CONTAINS 0.                                                   
        01 IN-POST                    PIC X(80).                                
            EJECT                                                               
                                                                                
       WORKING-STORAGE SECTION.                                                 
       77 IDPGM             PIC X(8)    VALUE 'W0156800'.                       
       77 JA                PIC X       VALUE 'J'.                              
       77 NEJ               PIC X       VALUE 'N'.                              
       77 EOF-INPOST        PIC X       VALUE 'N'.                              
       77 W-INANTAL         PIC S9(7)   VALUE +0   COMP-3.                      
       77 W-21POST          PIC S9(7)   VALUE +0   COMP-3.                      
       77 W-21SEG           PIC S9(7)   VALUE +0   COMP-3.                      
       77 W-SEGKEY      PIC S9(9)   VALUE +0   COMP-3.                          
       77 SKRIV-SW                 PIC X       VALUE 'N'.                       
       77 INDX          PIC S9(9)   VALUE ZERO COMP SYNC.                       
       77 TAB-IX        PIC S9(9)   VALUE ZERO COMP SYNC.                       
       77 21-IX         PIC S9(9)   VALUE ZERO COMP SYNC.                       
       77 W-IX          PIC S9(3)   VALUE ZERO COMP SYNC.                       
                                                                                
       01 INAREA.                                                               
      *    02  -COPY W479E4 -PRE IN-                                            
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
      *  -- CHECKED BY WY2000                                                   
       77 IDPGM           PIC X(8)    VALUE 'W0156800'.                         
       01 W-COUNT         PIC S9(9) COMP-3 VALUE +0.                            
       01 CHKP-VAR.                                                             
         03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9) VALUE +32 COMP.                  
           03  CHKP-MSG-IO-AREA         PIC X(32) VALUE SPACE.                  
           03 CHKP-AREA-LENGTH PIC S9(9) VALUE +32 COMP SYNC.                   
           03 CHKP-AREA    PIC X(32)   VALUE SPACE.                             
           03 CHKP-ANT     PIC S9(3)  VALUE +0   COMP-3.                        
           03 CHKP-MAX     PIC S9(3)  VALUE +300 COMP-3.                        
       01 WS-TS.                                                                
           03 WS-TS-TIAAMMDD           PIC 9(6).                                
           03 WS-TS-TIKLOCK            PIC 9(8).                                
       01 WS-TIKLOCK-X.                                                         
           03 WS-TIKLOCK-HHMMSS        PIC 9(6).                                
           03 FILLER                   PIC X(2).                                
           SKIP2                                                                
       01 FELTEXT.                                                              
           03  FILLER       PIC X(8)    VALUE 'FELTEXT'.                        
           03  FELTEXT-STR  PIC X(72)   VALUE SPACE.                            
                                                                                
       77 W01568-I-EOF-SW PIC X       VALUE 'N'.                                
           88  END-OF-W01568-I          VALUE 'J'.                              
       77 W-W01568-KVPOST-IN         PIC S9(7)   COMP-3.                        
       77 W-W01568-KVPOST            PIC S9(7)   COMP-3.                        
           EJECT                                                                
       01 DAGENS-DATUM    PIC 9(6)    VALUE ZERO.                               
       01 FILLER REDEFINES DAGENS-DATUM.                                        
           03  DAGENS-DATUM-AAR        PIC 9(2).                                
           03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
           03  DAGENS-DATUM-DAG        PIC 9(2).                                
           EJECT                                                                
       01 DYNAMISKA-SUBPROGRAM.                                                 
      *                                                                         
           03  CBLTDLI      PIC X(8)    VALUE 'CBLTDLI '.                       
           03  FELLOG       PIC X(8)    VALUE 'FELLOG  '.                       
           03  POSTSUM      PIC X(8)    VALUE 'POSTSUM '.                       
           SKIP3                                                                
       01 COUNTERS.                                                             
      *                                                                         
           03  W-COUNT1     PIC S9(7) COMP-3 VALUE ZERO.                        
           03  W-COUNT2     PIC S9(7) COMP-3 VALUE ZERO.                        
           03  W-COUNT3     PIC S9(7) COMP-3 VALUE ZERO.                        
           03  W-COUNT4     PIC S9(7) COMP-3 VALUE ZERO.                        
           03  W-COUNT5     PIC S9(7) COMP-3 VALUE ZERO.                        
            03  W-COUNT6     PIC S9(7) COMP-3 VALUE ZERO.                       
           03  W-COUNT-FLEJ PIC S9(7) COMP-3 VALUE ZERO.                        
        01 UT-AREA-START       PIC X(24)   VALUE                                
                                'UT-AREA-START'.                                
           SKIP2                                                                
        01 UT-AREA.                                                             
          03  UT-AREA-0.                                                        
           05  UT-IDPTYP           PIC X(3).                                    
           05  UT-ID               PIC X(8).                                    
           05  FILLER              PIC X(400).                                  
       01 RESTRT-AREA-START         PIC X(24)   VALUE                           
                            'RESTRT-AREA-START'.                                
           SKIP2                                                                
       01 RESTRT-AREA.                                                          
           03  RESTRT-AREA-0.                                                   
             05  RESTRT-IDPTYP       PIC X(3).                                  
             05  RESTRT-ID           PIC X(8).                                  
             05  FILLER               PIC X(400).                               
           EJECT                                                                
       01 FILLER         PIC X(16)   VALUE 'IMS-WS'.                            
       01 NYCKLAR-TILL-DLI.                                                     
           03  W-FILLER-A              PIC X(12).                               
           03  W-SEGKEY-X.                                                      
             05 W-IDDISTR  PIC S9(5)           COMP-3.                          
             05 W-IDKUNDNR PIC S9(7)           COMP-3.                          
             05 W-IDKUNDRF PIC X(10).                                           
             05 W-IDPRODNR PIC S9(7)           COMP-3.                          
             05 W-IDPLKLST PIC S9(3)           COMP-3.                          
           03  W-ROTKEY-X.                                                      
            05  W-KDSEGKEY PIC X(1)    VALUE SPACE.                             
      *    03  W-KDSEGKEY-21X.                                                  
      *     05  W-DAPRLIST          PIC S9(8).                                  
      *     05  W-FLHUVLEV          PIC X.                                      
      *    03  W-FILLER-X              PIC X(48).                               
           SKIP2                                                                
      * --- STATUS-KOD FRÅN IMS                                                 
       01 SW-BMP                 PIC X.                                         
           88  BMP                      VALUE 'J'.                              
       01 STATUS-WS                 PIC XX.                                     
           88  SEGMENT-FINNS            VALUE '  '.                             
           88  SEGMENT-FINNS-REDAN      VALUE 'II'.                             
           88  SEGMENT-SAKNAS           VALUE 'GE'.                             
           88  SEGMENT-SLUT             VALUE 'GB'.                             
           88  IMS-EJ-OK                VALUE 'XD'.                             
       01 STATUS-WS2                PIC XX.                                     
           88  SEGMENT1-FINNS          VALUE '  '.                              
           88  SEGMENT1-SLUT             VALUE 'GB'.                            
           SKIP2                                                                
       01 GODK-STATUSKODER.                                                     
         03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
           SKIP3                                                                
       01 SSA1                      PIC X(64).                                  
       01 SSA2                      PIC X(64).                                  
       01 SSA3                      PIC X(64).                                  
       01 VIMSREGT       PIC X(8)    VALUE 'VIMSREGT'.                          
       01 W-POSTSUM-AREA.                                                       
      *    03  -COPY W0005 -PRE POSTSUM-                                        
           EJECT                                                                
      * --- IMS FUNKTIONSKODER                                                  
      *01 -COPY W0003                                                           
           EJECT                                                                
      * ---  DLI INPUT-OUTPUT AREA                                              
                                                                                
       01 FILLER       PIC X(16) VALUE 'DLI-IO-SEG   '.                         
       01 DLI-IO-WDE401.                                                        
      *    03  -COPY WDE401                                                     
           EJECT                                                                
                                                                                
      * ---  DLI INPUT-OUTPUT AREA FÖR CHECKPOINT-HÄNDELSEBAS                   
                                                                                
       01 FILLER      PIC X(24)  VALUE 'DLI-IO-457901'.                         
       01 DLI-IO-AREA-2.                                                        
      *    03  -COPY WDGX4580.                                                  
           SKIP3                                                                
       01 FILLER      PIC X(24)  VALUE 'DLI-IO-4579NN'.                         
      *                                                                         
      *                                                                         
       01 FILLER     PIC X(24)  VALUE 'DLI-IO-457901'.                          
          EJECT                                                                 
       LINKAGE SECTION.                                                         
                                                                                
      *01 -COPY W0009 -PRE MSG-                                                 
                                                                                
      *01 -COPY W0008 -PRE WDE4-                                                
            05  FILLER                  PIC X.                                  
                                                                                
      *                                                                         
                                                                                
           EJECT                                                                
       PROCEDURE DIVISION USING MSG-PCB WDE4-PCB .                              
       MAIN SECTION.                                                            
           ENTRY 'DLITCBL' USING MSG-PCB WDE4-PCB .                             
                                                                                
           SKIP2                                                                
           PERFORM A-INIT                                                       
           PERFORM B-LAES-POST                                                  
           PERFORM UNTIL EOF-INPOST = JA                                        
           IF BMP AND                                                           
               CHKP-ANT > CHKP-MAX                                              
               PERFORM X-TAG-CHECKPOINT                                         
           END-IF                                                               
           MOVE IN-IDDISTR TO W-IDDISTR                                         
           MOVE IN-IDKUNDNR TO W-IDKUNDNR                                       
           MOVE IN-IDKUNDRF TO W-IDKUNDRF                                       
           MOVE IN-IDPRODNR TO W-IDPRODNR                                       
           MOVE IN-IDPLKLST TO W-IDPLKLST                                       
      * MOVE IN-FLHUVLEV TO W-FLHUVLEV                                          
           PERFORM IMS-GET-GU-SEG01                                             
           ADD +1 TO W-COUNT                                                    
      *    MOVE 'P' TO ODEL-KDODELSTA                                           
      *    MOVE 030221 TO ODEL-TIPACKN                                          
      *    MOVE 090000 TO ODEL-TIPACTID                                         
      *    MOVE ODEL-KVRADER TO ODEL-KVPACKRAD-OD                               
           ADD +1 TO CHKP-ANT                                                   
           PERFORM IMS-DLET-SEG01                                               
           PERFORM B-LAES-POST                                                  
           END-PERFORM                                                          
                                                                                
                                                                                
           PERFORM Z-FINIT                                                      
                                                                                
           MOVE ZERO TO RETURN-CODE                                             
           GOBACK                                                               
           .                                                                    
           EJECT                                                                
       A-INIT SECTION.                                                          
           SKIP2                                                                
                                                                                
           OPEN INPUT INDATA                                                    
           MOVE SPACE     TO SW-BMP                                             
           CALL VIMSREGT                                                        
           IF RETURN-CODE = +8                                                  
            MOVE 'J'       TO SW-BMP                                            
            PERFORM IMS-RESTART                                                 
                                                                                
                                                                                
      *    MOVE '4579' TO 4579-IDHTYP                                           
      *    MOVE 'W0156800' TO 4579-IDPGM                                        
      *    MOVE LOW-VALUE TO 4579-LOW-VALUE                                     
      *    PERFORM IMS-LAES-ATERSTART-4579                                      
                                                                                
      *    IF SEGMENT-FINNS                                                     
      *      IF 4580-KVPOST > ZERO                                              
      *      PERFORM AA-LAES-FRAM-FILER                                         
      *     PERFORM IMS-GET-ARTC-PAB                                            
      *     PERFORM AB-LAES-FRAM-FILER                                          
      *     END-IF                                                              
      *    ELSE                                                                 
      *    PERFORM IMS-ISRT-4579                                                
      *    END-IF                                                               
           END-IF                                                               
           .                                                                    
           EJECT                                                                
      *AA-LAES-FRAM-FILER SECTION.                                              
      *                                                                         
      *    MOVE 4580-FILLERX66 TO NYCKLAR-TILL-DLI                              
      *    PERFORM IMS-GET-GU-SEG01                                             
           .                                                                    
           EJECT                                                                
       AB-LAES-FRAM-FILER SECTION.                                              
           PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS                         
      *   IF WDQ2-SEG-NAME-FB = 'WDQ211  '                                      
      *     PERFORM IMS-ISRT-K612                                               
      *     PERFORM IMS-ISRT-K613                                               
            ADD +1 TO CHKP-ANT                                                  
      *   END-IF                                                                
      *   MOVE 'N' TO ARTC-CLAG-FLEJBUFF                                        
      *   ADD +1 TO W-COUNT-FLEJ                                                
      *   PERFORM IMS-REPL-ARTC-PAF                                             
      *   PERFORM IMS-GET-ARTC-PAB                                              
            END-PERFORM                                                         
            .                                                                   
            EJECT                                                               
       B-LAES-POST SECTION.                                                     
                                                                                
            READ INDATA                                                         
            AT END                                                              
            MOVE JA TO EOF-INPOST                                               
            NOT AT END                                                          
            ADD +1 TO W-INANTAL                                                 
            MOVE IN-POST TO INAREA                                              
            END-READ                                                            
            .                                                                   
            EJECT                                                               
        Z-FINIT SECTION.                                                        
                                                                                
      *      IF BMP                                                             
      *     MOVE '4579' TO 4579-IDHTYP                                          
      *     MOVE LOW-VALUE TO 4579-LOW-VALUE                                    
      *     PERFORM IMS-LAES-ATERSTART-4579                                     
      *     MOVE 0 TO 4580-KVPOST                                               
      *     ACCEPT 4580-TIUPPDAT FROM DATE                                      
      *     ACCEPT 4580-TIUPPTID FROM TIME                                      
      *     PERFORM IMS-REPL-ATERSTART-4579                                     
      *     END-IF                                                              
            DISPLAY ' NYA WDGX4579     : ' W-COUNT3                             
             DISPLAY ' LÄSTA RÖTTER     : ' W-COUNT                             
            DISPLAY ' LÄSTA 11-SEGMENT : ' W-COUNT5                             
             DISPLAY ' II-STATUS        : ' W-COUNT6                            
            DISPLAY ' ÄNDRADE SEGMENT  : ' W-COUNT-FLEJ                         
            CLOSE INDATA                                                        
            .                                                                   
            EJECT                                                               
       X-TAG-CHECKPOINT SECTION.                                                
                                                                                
      * --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSE                        
      * --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
      *    MOVE '4579' TO  4579-IDHTYP                                          
      *    MOVE LOW-VALUE TO 4579-LOW-VALUE                                     
      *    PERFORM IMS-LAES-ATERSTART-4579                                      
      * BYT UT W01568 TILL MOTSVARANDE INFIL                                    
      *    ACCEPT 4580-TIUPPDAT FROM DATE                                       
      *    ACCEPT 4580-TIUPPTID FROM TIME                                       
      * MOVE ARTC-ART-IDARTNR TO W-IDARTNR                                      
      * MOVE ARTC-CLAG-KDSEGKEY TO W-KDSEGKEY                                   
      *    MOVE NYCKLAR-TILL-DLI TO 4580-FILLERX66                              
      * PERFORM IMS-REPL-ATERSTART-4579                                         
                                                                                
           PERFORM IMS-CHECKPOINT                                               
           MOVE ZERO TO CHKP-ANT                                                
      * --- LÄS OM DATABAS OM DET BEHÖVS                                        
      * PERFORM IMS-GET-GU-WDQ201                                               
           .                                                                    
           EJECT                                                                
      * --- IMS SEKTIONER ---                                                   
                                                                                
      *IMS-LAES-ATERSTART-4579 SECTION.                                         
                                                                                
      *    STRING 'WL457901(WDGXKEY  =' 4579-WDGX4579-CTX ')'                   
      *     DELIMITED BY SIZE INTO SSA1                                         
      *    MOVE 'WL457911(KDSEGKEY =1)' TO SSA2                                 
      *    MOVE '  GE' TO GODK-STATUSKODER                                      
      *    CALL CBLTDLI USING GHU 4579-PCB DLI-IO-AREA-2                        
      *        SSA1 SSA2                                                        
      *      MOVE 4579-STATUS-CODE TO STATUS-WS                                 
      *    PERFORM IMS-STATUSKONTROLL.                                          
      *    EJECT                                                                
      *IMS-REPL-ATERSTART-4579 SECTION.                                         
                                                                                
      *    MOVE '  ' TO GODK-STATUSKODER                                        
      *    CALL CBLTDLI USING REPL 4579-PCB DLI-IO-AREA-2                       
      *    MOVE 4579-STATUS-CODE TO STATUS-WS                                   
      *    PERFORM IMS-STATUSKONTROLL                                           
      *    .                                                                    
           EJECT                                                                
       IMS-GET-GU-SEG01 SECTION.                                                
                                                                                
      *    STRING 'WDE401  (WDE401KY =' W-ROTKEY-X ')'                          
      *       DELIMITED BY SIZE INTO SSA1                                       
           STRING 'WDE401  (WDE401KY =' W-SEGKEY-X ')'                          
              DELIMITED BY SIZE INTO SSA2                                       
           MOVE '    ' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-WDE401 SSA2                   
           MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL.                                          
           EJECT                                                                
       IMS-DLET-SEG01 SECTION.                                                  
                                                                                
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-WDE401                       
           MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
      *IMS-ISRT-4579 SECTION.                                                   
                                                                                
      *    MOVE 'WL457901 ' TO SSA1                                             
      *    MOVE 'WL457911 ' TO SSA2                                             
      *    MOVE '4579' TO 4579-IDHTYP                                           
      *    MOVE 'W0156800' TO 4579-IDPGM                                        
      *    MOVE LOW-VALUE TO 4579-LOW-VALUE                                     
      *    MOVE '  II' TO GODK-STATUSKODER                                      
      *    ACCEPT WS-TS-TIAAMMDD FROM DATE                                      
      *    ACCEPT WS-TS-TIKLOCK FROM TIME                                       
      *    MOVE WS-TS-TIAAMMDD TO 4580-TIUPPDAT                                 
      *    MOVE WS-TS-TIKLOCK TO 4580-TIUPPTID                                  
      *    MOVE '1' TO 4580-KDSEGKEY                                            
      *    MOVE ZERO TO 4580-KVPOST                                             
      *    CALL CBLTDLI USING ISRT 4579-PCB DLI-IO-AREA-1 SSA1                  
      *    MOVE 4579-STATUS-CODE TO STATUS-WS                                   
      *    PERFORM IMS-STATUSKONTROLL                                           
      *                                                                         
      *    MOVE '  ' TO GODK-STATUSKODER                                        
      *    ADD +1 TO W-COUNT3                                                   
      *    STRING 'WL457901(WDGXKEY  =' 4579-WDGX4579-CTX ')'                   
      *     DELIMITED BY SIZE INTO SSA1                                         
      *     CALL CBLTDLI USING ISRT 4579-PCB DLI-IO-AREA-2                      
      *        SSA1 SSA2                                                        
      *    MOVE 4579-STATUS-CODE TO STATUS-WS                                   
      *    PERFORM IMS-STATUSKONTROLL                                           
      *   .                                                                     
           EJECT                                                                
      *IMS-GET-ARTC-PAB SECTION.                                                
      *                                                                         
      * MOVE 'WDQ211   ' TO SSA1                                                
      * STRING 'WDQ211(KDSEGKEY =' W-KDSEGKEY-X ')'                             
      *       DELIMITED BY SIZE INTO SSA1                                       
      * MOVE 'GKGE' TO GODK-STATUSKODER                                         
      * CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-WDQ211                          
      * MOVE WDQ2-STATUS-CODE TO STATUS-WS                                      
      *     ADD +1 TO W-COUNT5                                                  
      * PERFORM IMS-STATUSKONTROLL                                              
      * .                                                                       
      * SKIP3                                                                   
      * EJECT                                                                   
       IMS-RESTART SECTION.                                                     
           MOVE SPACE TO CHKP-MSG-IO-AREA                                       
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING XRST MSG-PCB                                      
               CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA                         
                     CHKP-AREA-LENGTH CHKP-AREA                                 
           MOVE MSG-STATUS-CODE TO STATUS-WS                                    
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
       IMS-CHECKPOINT SECTION.                                                  
           SKIP2                                                                
           MOVE SPACE TO CHKP-MSG-IO-AREA                                       
           MOVE '  XD' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING CHKP MSG-PCB                                      
                   CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA                     
                     CHKP-AREA-LENGTH CHKP-AREA                                 
           MOVE MSG-STATUS-CODE TO STATUS-WS                                    
           PERFORM IMS-STATUSKONTROLL                                           
                                                                                
           IF IMS-EJ-OK                                                         
           MOVE 'IMS-KONTROLLREGION EJ ÄNGLIG' TO FELTEXT-STR                   
           DISPLAY FELTEXT                                                      
           CALL FELLOG                                                          
           END-IF                                                               
           .                                                                    
           EJECT                                                                
       IMS-STATUSKONTROLL SECTION.                                              
           SKIP2                                                                
           IF STATUS-WS = 'II'                                                  
           ADD +1 TO W-COUNT6                                                   
           END-IF                                                               
           SET STATUS-IX TO 1                                                   
           SEARCH GODK-STATUS                                                   
           AT END                                                               
           STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                    
           DELIMITED BY SIZE INTO FELTEXT                                       
           DISPLAY FELTEXT                                                      
           CALL FELLOG                                                          
           WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
           CONTINUE                                                             
           END-SEARCH                                                           
           .                                                                    
                                                                                
