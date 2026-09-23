       ID DIVISION.                                                             
           SKIP2                                                                
       PROGRAM-ID.     W4160300.                                                
      *AUTHOR.         JAN-ERIK FRANTZEN.                                       
      *DATE-WRITTEN.   91/09/16.                                                
                                                                                
      *    REMARKS.                                                             
      *                                                                         
      *    FUNKTION:                                                            
      *        RENSAR SATSORDERBASEN WDJ201                                     
      *                                                                         
      *        PROGRAMMET UPPDATERAR WLSATG (WDJ2)                              
      *        PROGRAMMET LÄSER      WLSATJ (WDE2C1)                            
      *        PROGRAMMET LÄSER      WDE6                                       
      *        PROGRAMMET LÄSER      W6INLA (W6D1B1)                            
      *        PROGRAMMET LÄSER      WLINLB (WDD9)                              
      *                                                                         
      *    ABENDKODER:                                                          
      *        U0033 -  ABEND-MED-DUMP                                          
      *                                                                         
                                                                                
           SKIP3                                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
           SKIP2                                                                
      *    -- CHECKED BY WY2000                                                 
           SKIP3                                                                
       77  IDPGM                       PIC X(8)    VALUE 'W4160300'.            
       77  JA                          PIC X       VALUE 'J'.                   
       77  NEJ                         PIC X       VALUE 'N'.                   
       77  WS-TIBEGPAC-SPAR            PIC  9(6)   VALUE ZERO.                  
       77  WS-TIAAVV                   PIC  9(4)   VALUE ZERO.                  
       77  FIX-J2-DLET                 PIC  9(4)   VALUE ZERO.                  
       77  WS-TIAAVV-NUM               PIC S9(5)   VALUE +0 COMP-3.             
       77  IX                          PIC S9(3)   VALUE +0 COMP-3.             
       77  RKOD-ABEND-MED-DUMP         PIC S9(3)   VALUE +33 COMP-3.            
       77  SEGMENT-SLUT-WDJ2C1         PIC  X(1)   VALUE 'N'.                   
       77  WS-IDLOPNRM-SPAR            PIC 9(9)    VALUE ZERO.                  
       77  WS-IDLOPNRM-ALFA            PIC X(7)    VALUE SPACE.                 
                                                                                
      *01  -COPY WWDCKONS                                                       
                                                                                
       01  WS-IDLOPNRM.                                                         
          03  WS-IDLOPNRM-POS-1-2      PIC  9(2)   VALUE ZERO.                  
          03  WS-IDLOPNRM-POS-3-8      PIC  9(7)   VALUE ZERO.                  
                                                                                
       01  WS-IDLOPNRM-RED.                                                     
          03  WS-IDLOPNRM-1            PIC  9(1)   VALUE ZERO.                  
          03  WS-IDLOPNRM-2            PIC  9(7)   VALUE ZERO.                  
          03  WS-IDLOPNRM-3            PIC  9(1)   VALUE ZERO.                  
                                                                                
       01  WS-IDORDNST-GAMMAL.                                                  
          03  WS-IDORDNSB-GAMMAL       PIC S9(5)   VALUE +0 COMP-3.             
          03  WS-IDORDNSS-GAMMAL       PIC S9(1)   VALUE +0 COMP-3.             
                                                                                
       01  WS-IDORDNST-SPAR.                                                    
          03  WS-IDORDNSB-SPAR         PIC S9(5)   VALUE +0 COMP-3.             
          03  WS-IDORDNSS-SPAR         PIC S9(1)   VALUE +0 COMP-3.             
                                                                                
       01  ALLT-SW                     PIC  X(1)   VALUE 'J'.                   
          88  ALLT-OK                              VALUE 'J'.                   
                                                                                
       01  FELTEXT.                                                             
           03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
           03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
           EJECT                                                                
                                                                                
       01 IDORDNST-TABELL.                                                      
          03 WS-IDORDNST-TAB OCCURS 11 TIMES.                                   
              05 WS-IDORDNSB-TAB       PIC 9(5).                                
              05 WS-IDORDNSS-TAB       PIC 9(1).                                
           SKIP3                                                                
                                                                                
                                                                                
       01 IDPRODNR-TABELL.                                                      
          03 WDJ2-IDPRODNR-TABELL OCCURS 11 TIMES.                              
              05 WS-IDPRODNR-TAB       PIC 9(7).                                
           SKIP3                                                                
                                                                                
                                                                                
       01  IDLOPNRM-TABELL.                                                     
          03 W6D1B-IDLOPNRM-TABELL OCCURS 11 TIMES.                             
              05 WS-IDLOPNRM-TAB       PIC 9(9).                                
           SKIP3                                                                
                                                                                
       01  DYNAMISKA-SUBPROGRAM.                                                
      *                                                                         
           03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
           03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
           03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
           03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
           03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
           EJECT                                                                
      *01  -COPY WDATAREA                                                       
      *                                                                         
           EJECT                                                                
      *****************************************************                     
      *    DESSA FÄLT ÄR PARAMETRAR VID ANROP AV CHECK,   *                     
      *    SOM RÄKNAR UT KONTROLLSIFFRAN FÖR LÖPNUMMER    *                     
      *****************************************************                     
       01  FILLER                      PIC X(8)    VALUE 'CHECK'.               
                                                                                
       01  CHECK-PARM.                                                          
           03  LOPNR                   PIC  X(7).                               
           03  LGD                     PIC S9(4)   COMP SYNC VALUE +7.          
           03  WEIGHT                  PIC S9(7)   VALUE 2121212.               
           03  FIGURES                 PIC S9(4)   COMP SYNC VALUE +7.          
           03  KSIFFRA                 PIC  X(1).                               
           03  MODUL-10-11             PIC  9(2)   VALUE 10.                    
           03  ALT-A-B                 PIC  X(1)   VALUE 'B'.                   
      *                                                                         
           EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
           SKIP2                                                                
       01  NYCKLAR-TILL-DLI.                                                    
           03  W-IDORDNST-X.                                                    
               05  W-IDORDNSB          PIC S9(5)   VALUE ZERO COMP-3.           
               05  W-IDORDNSS          PIC S9      VALUE ZERO COMP-3.           
                                                                                
           03  W-WDJ2C1KY-X.                                                    
               05  W-IDARTNR-SEQC      PIC S9(9)   VALUE ZERO COMP-3.           
               05  W-DAREGDAT-SEQC     PIC  9(8)   VALUE ZERO.                  
               05  W-IDORDNST-SEQC .                                            
                  07 W-IDORDNSB-SEQC   PIC S9(5)   VALUE ZERO COMP-3.           
                  07 W-IDORDNSS-SEQC   PIC S9(1)   VALUE ZERO COMP-3.           
                                                                                
           03  W-IDPRODNR-X.                                                    
               05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
                                                                                
           03  W-W6D1BSEQ-X.                                                    
               05  W-W6D1BSEQ-IDLOPNRM PIC S9(9)   VALUE ZERO COMP-3.           
                                                                                
           03  W-WDD901KY-X.                                                    
               05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
               05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
                                                                                
           03  W-IDLEVNR-X.                                                     
               05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
                                                                                
           03  W-WDD905KY-X.                                                    
                05  W-DAAVROP-AVS   PIC  9(6)    VALUE ZERO.                    
                05  W-TILEVDAG      PIC  S9      VALUE +1 COMP-3.               
           03  W-KDAVROP-X.                                                     
                05  W-KDAVROP       PIC  S9      VALUE +9 COMP-3.               
                                                                                
           SKIP2                                                                
      *    --- STATUS-KOD FRÅN IMS                                              
       01  STATUS-WS                   PIC XX.                                  
           88  SEGMENT-FINNS                       VALUE '  '.                  
           88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
           88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
           88  SEGMENT-SLUT                        VALUE 'GB'.                  
           88  IMS-EJ-OK                           VALUE 'XD'.                  
           SKIP2                                                                
       01  GODK-STATUSKODER.                                                    
           03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
           SKIP3                                                                
       01  SSA1                        PIC X(64).                               
       01  SSA2                        PIC X(64).                               
       01  SSA3                        PIC X(64).                               
           EJECT                                                                
      *    --- IMS FUNKTIONSKODER                                               
      *01  -COPY W0003                                                          
           EJECT                                                                
      *    ---  DLI INPUT-OUTPUT AREA                                           
       01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
           SKIP3                                                                
       01  DLI-IO-AREA-WDJ2C1.                                                  
      *    03  -COPY WDJ2C1                                                     
           SKIP3                                                                
       01  DLI-IO-AREA.                                                         
           03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
           SKIP3                                                                
           03  WLSATG01 REDEFINES IO-AREA.                                      
      *        05  -COPY WDJ201                                                 
           SKIP3                                                                
           03  WDE601   REDEFINES IO-AREA.                                      
      *        05  -COPY WDE601                                                 
           SKIP3                                                                
           03  W6INLB   REDEFINES IO-AREA.                                      
      *        05  -COPY W6D111  -PRE INLA-                                     
           SKIP3                                                                
           03  WLINLB23 REDEFINES IO-AREA.                                      
      *        05  -COPY WDD905  -PRE INLB-                                     
           SKIP3                                                                
           03  WLINLB31 REDEFINES IO-AREA.                                      
      *        05  -COPY WDD906  -PRE INLB-                                     
           EJECT                                                                
       LINKAGE SECTION.                                                         
                                                                                
      *01  -COPY W0009   -PRE MSG-                                              
           EJECT                                                                
      *01  -COPY W0008  -PRE SATG-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
      *01  -COPY W0008  -PRE SATJ-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
      *01  -COPY W0008  -PRE WDE6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
      *01  -COPY W0008  -PRE W6INLA-                                            
           05  FILLER                  PIC X.                                   
           EJECT                                                                
      *01  -COPY W0008  -PRE INLB-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
       PROCEDURE DIVISION  USING MSG-PCB SATG-PCB SATJ-PCB WDE6-PCB             
           W6INLA-PCB INLB-PCB.                                                 
           ENTRY 'DLITCBL' USING MSG-PCB SATG-PCB SATJ-PCB WDE6-PCB             
           W6INLA-PCB INLB-PCB.                                                 
                                                                                
           PERFORM A-INIT                                                       
           PERFORM IMS-GU-SATJ-WDJ2C                                            
           IF SEGMENT-SAKNAS                                                    
              CONTINUE                                                          
           ELSE                                                                 
              PERFORM B-HUVUDSLINGA                                             
           END-IF                                                               
           DISPLAY 'ANT-J2-DLET '  FIX-J2-DLET                                  
           MOVE ZERO TO RETURN-CODE                                             
           GOBACK                                                               
           .                                                                    
           EJECT                                                                
       A-INIT SECTION.                                                          
                                                                                
           MOVE LOW-VALUE                     TO W-IDORDNST-X                   
                                                 W-WDJ2C1KY-X                   
                                                 W-IDPRODNR-X                   
                                                 W-WDD901KY-X                   
           MOVE SPACE                         TO W-IDLEVNR-X                    
                                                                                
           .                                                                    
           EJECT                                                                
       B-HUVUDSLINGA SECTION.                                                   
                                                                                
           PERFORM UNTIL SEGMENT-SLUT-WDJ2C1 = JA                               
              MOVE JA                         TO ALLT-SW                        
              PERFORM BA-LAES-FRAM-WDJ2C                                        
              MOVE +1                         TO IX                             
              PERFORM UNTIL WS-IDORDNSB-TAB(IX) = ZERO  OR                      
                                    NOT ALLT-OK                                 
                                                                                
                 IF SHUV-KDSATSTA = 'U'                                         
                    IF IX = +1                                                  
                       MOVE SHUV-IDARTNR      TO W-IDARTNR                      
                       MOVE SHUV-IDLEVNR      TO W-IDLEVNR                      
                       MOVE SHUV-TIBEGPAC     TO WS-TIBEGPAC-SPAR               
                    END-IF                                                      
                    MOVE SHUV-IDPRODNR        TO WS-IDPRODNR-TAB(IX)            
                    ADD +1                    TO IX                             
                    IF WS-IDORDNSB-TAB(IX) > ZERO                               
                       MOVE WS-IDORDNSB-TAB(IX) TO W-IDORDNSB                   
                       MOVE WS-IDORDNSS-TAB(IX) TO W-IDORDNSS                   
                       PERFORM IMS-GU-SATG-WDJ2                                 
                    END-IF                                                      
                 ELSE                                                           
                    MOVE NEJ                  TO ALLT-SW                        
                 END-IF                                                         
              END-PERFORM                                                       
              IF ALLT-OK                                                        
                 PERFORM C-BEHANDLA-ORDERN                                      
              END-IF                                                            
           END-PERFORM                                                          
           .                                                                    
           EJECT                                                                
       BA-LAES-FRAM-WDJ2C SECTION.                                              
                                                                                
           PERFORM BAA-NOLLA-TABELLER                                           
           MOVE +1                            TO IX                             
           MOVE +0                            TO WS-TIBEGPAC-SPAR               
           MOVE SEQC-IDORDNST                 TO WS-IDORDNST-SPAR               
           PERFORM UNTIL SEQC-IDORDNSB NOT = WS-IDORDNSB-SPAR OR                
                                             SEGMENT-SLUT                       
              MOVE SEQC-IDORDNSB              TO  WS-IDORDNSB-TAB(IX)           
              MOVE SEQC-IDORDNSS              TO  WS-IDORDNSS-TAB(IX)           
              PERFORM IMS-GN-SATJ-WDJ2C                                         
              ADD +1                          TO IX                             
           END-PERFORM                                                          
                                                                                
           IF SEGMENT-SLUT                                                      
              MOVE JA                         TO SEGMENT-SLUT-WDJ2C1            
           END-IF                                                               
                                                                                
           MOVE +1                            TO IX                             
           MOVE WS-IDORDNSB-TAB(IX)           TO W-IDORDNSB                     
           MOVE WS-IDORDNSS-TAB(IX)           TO W-IDORDNSS                     
           PERFORM IMS-GU-SATG-WDJ2                                             
           .                                                                    
           EJECT                                                                
       BAA-NOLLA-TABELLER SECTION.                                              
                                                                                
           MOVE +1                            TO IX                             
           PERFORM 11 TIMES                                                     
              MOVE ZERO                       TO WS-IDPRODNR-TAB(IX)            
                                                 WS-IDLOPNRM-TAB(IX)            
                                                 WS-IDORDNSB-TAB(IX)            
                                                 WS-IDORDNSS-TAB(IX)            
              ADD +1                          TO IX                             
           END-PERFORM                                                          
                                                                                
           MOVE +1                            TO IX                             
                                                                                
           .                                                                    
           EJECT                                                                
       C-BEHANDLA-ORDERN SECTION.                                               
                                                                                
           MOVE JA                            TO ALLT-SW                        
           PERFORM CA-LAES-KOLLA-WDE601                                         
           IF ALLT-OK                                                           
              PERFORM CB-LAES-KOLLA-WDD9                                        
              IF ALLT-OK                                                        
                 PERFORM CC-LAES-KOLLA-W6D1                                     
                 IF ALLT-OK                                                     
                    PERFORM CD-TA-BORT-ORDERN-FRAN-WDJ2                         
                 END-IF                                                         
              END-IF                                                            
           END-IF                                                               
           .                                                                    
           EJECT                                                                
       CA-LAES-KOLLA-WDE601 SECTION.                                            
                                                                                
           MOVE +1                            TO IX                             
           MOVE 'JF'                          TO STATUS-WS                      
                                                                                
           PERFORM UNTIL IX > +10 OR                                            
                    SEGMENT-FINNS OR                                            
                    WS-IDPRODNR-TAB(IX) = ZERO                                  
              MOVE WS-IDPRODNR-TAB(IX)        TO W-IDPRODNR                     
              PERFORM IMS-GU-WDE601                                             
              IF SEGMENT-FINNS                                                  
                 MOVE NEJ                     TO ALLT-SW                        
              END-IF                                                            
              ADD +1                          TO IX                             
           END-PERFORM                                                          
           .                                                                    
           EJECT                                                                
       CB-LAES-KOLLA-WDD9 SECTION.                                              
                                                                                
           MOVE 'AAMMDD'                      TO DAT-KDDATFORM                  
           MOVE WS-TIBEGPAC-SPAR              TO DAT-I-TIDATUM                  
           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
                           DAT-O-TIDATUM DAT-KDSVAR                             
                                                                                
           IF DAT-KDSVAR-OK                                                     
              MOVE DAT-TIAAVV-GRP             TO WS-TIAAVV                      
              MOVE WS-TIAAVV                  TO WS-TIAAVV-NUM                  
           ELSE                                                                 
              MOVE 'FEL SVAR FRÅN DATKONV'    TO FELTEXT-STR                    
              CALL ABEND USING RKOD-ABEND-MED-DUMP                              
           END-IF                                                               
                                                                                
           MOVE WS-TIAAVV-NUM              TO W-DAAVROP-AVS                     
           MOVE DAT-TISEKEL                TO W-DAAVROP-AVS(1:2)                
           MOVE WC-CDC-SE                  TO W-IDDC                            
           PERFORM IMS-GU-INLB-WDD905                                           
           MOVE +1                            TO IX                             
           IF SEGMENT-FINNS                                                     
             PERFORM UNTIL SEGMENT-SAKNAS                                       
               PERFORM IMS-GNP-INLB-WDD9                                        
               IF SEGMENT-FINNS                                                 
                 MOVE INLB-IDLOPNRM-PL  TO WS-IDLOPNRM-TAB(IX)                  
                 ADD +1                 TO IX                                   
               END-IF                                                           
             END-PERFORM                                                        
           ELSE                                                                 
              DISPLAY 'EJ-D905 ' W-IDARTNR ' / ' W-IDLEVNR                      
           END-IF                                                               
           .                                                                    
           EJECT                                                                
       CC-LAES-KOLLA-W6D1 SECTION.                                              
                                                                                
           MOVE +1                            TO IX                             
                                                                                
           PERFORM UNTIL WS-IDLOPNRM-TAB(IX) = ZERO OR                          
                                     ALLT-SW = NEJ                              
                                                                                
              IF ALLT-OK                                                        
                 MOVE WS-IDLOPNRM-TAB(IX)     TO WS-IDLOPNRM                    
                 MOVE WS-IDLOPNRM-POS-3-8     TO WS-IDLOPNRM-ALFA               
                 MOVE WS-IDLOPNRM-ALFA        TO LOPNR                          
                 PERFORM CCA-RAKNA-UT-CHECKSIFFRA                               
                 MOVE WS-IDLOPNRM-POS-3-8     TO WS-IDLOPNRM-2                  
                 MOVE WS-IDLOPNRM-RED         TO WS-IDLOPNRM-SPAR               
                                                                                
                 MOVE WS-IDLOPNRM-SPAR        TO W-W6D1BSEQ-IDLOPNRM            
                                                                                
                 PERFORM IMS-GU-W6INLA-ART                                      
                 IF SEGMENT-FINNS                                               
                    IF INLA-ART-FLKLAR = JA                                     
                       CONTINUE                                                 
                    ELSE                                                        
                       MOVE NEJ               TO ALLT-SW                        
                    END-IF                                                      
                 END-IF                                                         
                 ADD +1                       TO IX                             
              END-IF                                                            
           END-PERFORM                                                          
           .                                                                    
           EJECT                                                                
       CCA-RAKNA-UT-CHECKSIFFRA SECTION.                                        
                                                                                
           CALL CHECK USING LOPNR LGD WEIGHT FIGURES KSIFFRA                    
                            MODUL-10-11 ALT-A-B                                 
                                                                                
           MOVE KSIFFRA                TO WS-IDLOPNRM-3                         
                                                                                
           .                                                                    
           EJECT                                                                
       CD-TA-BORT-ORDERN-FRAN-WDJ2 SECTION.                                     
                                                                                
           MOVE +1                            TO IX                             
           PERFORM UNTIL WS-IDORDNST-TAB(IX) = ZERO                             
              MOVE WS-IDORDNSB-TAB(IX) TO W-IDORDNSB                            
              MOVE WS-IDORDNSS-TAB(IX) TO W-IDORDNSS                            
              PERFORM IMS-GHU-SATG-SHUV                                         
              PERFORM IMS-DLET-SATG                                             
              ADD +1                         TO IX                              
                       FIX-J2-DLET                                              
           END-PERFORM                                                          
           .                                                                    
           EJECT                                                                
      * --- IMS SEKTIONER ---                                                   
           SKIP3                                                                
           EJECT                                                                
       IMS-GU-SATJ-WDJ2C SECTION.                                               
                                                                                
           STRING 'WLSATJ01(WDJ2C1KY >' W-WDJ2C1KY-X ')'                        
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU SATJ-PCB DLI-IO-AREA-WDJ2C1 SSA1               
           MOVE SATJ-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
       IMS-GN-SATJ-WDJ2C SECTION.                                               
                                                                                
           STRING 'WLSATJ01(WDJ2C1KY >' W-WDJ2C1KY-X ')'                        
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GB' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GN SATJ-PCB DLI-IO-AREA-WDJ2C1 SSA1               
           MOVE SATJ-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
       IMS-GU-SATG-WDJ2 SECTION.                                                
                                                                                
           STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  '   TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU SATG-PCB DLI-IO-AREA SSA1                      
           MOVE SATG-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
       IMS-GHU-SATG-SHUV SECTION.                                               
                                                                                
           STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GHU SATG-PCB DLI-IO-AREA SSA1                     
           MOVE SATG-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
       IMS-DLET-SATG SECTION.                                                   
                                                                                
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING DLET SATG-PCB DLI-IO-AREA                         
           MOVE SATG-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
       IMS-GU-WDE601    SECTION.                                                
                                                                                
           STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA SSA1                      
           MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
       IMS-GU-W6INLA-ART SECTION.                                               
           STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU W6INLA-PCB DLI-IO-AREA SSA1                    
           MOVE W6INLA-STATUS-CODE TO STATUS-WS                                 
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
       IMS-GU-INLB-WDD905 SECTION.                                              
                                                                                
           STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
                DELIMITED BY SIZE INTO SSA1                                     
           STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
                DELIMITED BY SIZE INTO SSA2                                     
           STRING 'WLINLB23(WDD905KY =' W-WDD905KY-X                            
                          '&KDAVROP  =' W-KDAVROP-X ')'                         
                DELIMITED BY SIZE INTO SSA3                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3            
           MOVE INLB-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
       IMS-GNP-INLB-WDD9 SECTION.                                               
                                                                                
           MOVE 'WLINLB31 ' TO SSA1                                             
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
           MOVE INLB-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
       IMS-STATUSKONTROLL SECTION.                                              
           SKIP2                                                                
           SET STATUS-IX TO 1                                                   
           SEARCH GODK-STATUS                                                   
             AT END                                                             
               MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT-STR                     
               DISPLAY FELTEXT                                                  
               CALL FELLOG                                                      
             WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
           END-SEARCH                                                           
           .                                                                    
           EJECT                                                                
