000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0154100.                                                
000300 AUTHOR.         CHRISTINA BRUHN.                                         
000400 DATE-WRITTEN.   90/11/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        RENSAR ÅTERSTARTSDATABASEN WDG8                                  
000900*        LISTA TAS BORT OM ANTAL ARBETSDAGAR ENLIGT WORKDAY               
001000*        ÄR FLER ÄN ANTAL I STYRPOSTEN.                                   
001100                                                                          
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600                                                                          
001700     SELECT STYRIN      ASSIGN TO W01541D1.                               
001800                                                                          
001900 DATA DIVISION.                                                           
002000 FILE SECTION.                                                            
002100 FD  STYRIN                                                               
002200     LABEL RECORD STANDARD                                                
002300     RECORDING F                                                          
002400     BLOCK CONTAINS 0.                                                    
002500                                                                          
002600 01  STYRPOST                    PIC X(80).                               
002700                                                                          
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200                                                                          
003300 77  IDPGM                       PIC X(08)   VALUE 'W0154100'.            
003400 77  W-COMPILED                  PIC X(16)   VALUE SPACE.                 
003500 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  STYR-EOF                    PIC X       VALUE 'N'.                   
003900 77  WS-DATE                     PIC 9(6)    VALUE ZERO.                  
004000 77  WS-TIREGDAT-SPAR            PIC 9(6)    VALUE ZERO.                  
004100 77  WS-KVANTAL                  PIC S9(7)   VALUE +0  COMP-3.            
004200 77  WS-CHKP-RAEKNARE            PIC S9(3)   VALUE +0  COMP-3.            
004300 77  CHKP-ID                     PIC X(8)    VALUE 'W0154100'.            
004400 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
004500 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
004600 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
004700 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
004800                                                                          
004900*    --- PARAMETRAR TILL ABEND                                            
005000 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
005100                                                                          
005200                                                                          
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005600   03  WORKDAY                   PIC X(8)    VALUE 'WORKDAY '.            
005700                                                                          
005800 01  IN-STYRAREA.                                                         
005900   03  IN-KVARBDAG               PIC 9(3)    VALUE 003.                   
006000   03  FILLER                    PIC X(77)   VALUE SPACE.                 
006100                                                                          
006200     EJECT                                                                
006300*      --- VALID IDDC CODES                                               
006400*                                                                         
006500*01    -COPY WWDCKONS                                                     
660033                                                                          
670000     EJECT                                                                
680025*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
690025*01  -COPY WORKAREA                                                       
700033                                                                          
710000     EJECT                                                                
720000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
730021                                                                          
740000 01  NYCKLAR-TILL-DLI.                                                    
750033   03  W-TIREGDAT-X.                                                      
760033     05  W-TIREGDAT-ASEQ         PIC S9(7)   VALUE +0 COMP-3.             
770038                                                                          
780033   03  W-WDG8A1KY-X.                                                      
790033     05  W-IDLTERM-A1            PIC X(8)    VALUE SPACE.                 
800033     05  W-TIREGDAT-A1           PIC S9(7)   VALUE +0 COMP-3.             
810033     05  W-TIKLOCK-9KOMPL-A1     PIC S9(9)   VALUE +0 COMP-3.             
820021                                                                          
830033   03  W-WDG801KY-X.                                                      
840033     05  W-IDLTERM               PIC X(8)    VALUE SPACE.                 
850033     05  W-TIREGDAT              PIC S9(7)   VALUE +0 COMP-3.             
860033     05  W-TIKLOCK-9KOMPL        PIC S9(9)   VALUE +0 COMP-3.             
870021                                                                          
880021                                                                          
890000*    --- STATUS-KOD FRÅN IMS                                              
900000 01  STATUS-WS                   PIC XX.                                  
910000     88  SEGMENT-FINNS                       VALUE '  '.                  
920000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
930000     88  BASEN-SLUT                          VALUE 'GB'.                  
940000     88  IMS-EJ-OK                           VALUE 'XD'.                  
950021                                                                          
960021                                                                          
970000 01  GODK-STATUSKODER.                                                    
980000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
990021                                                                          
000021                                                                          
010000 01  SSA1                        PIC X(64).                               
020030                                                                          
030000     EJECT                                                                
040000*    --- IMS FUNKTIONSKODER                                               
050001*01  -COPY W0003                                                          
060033                                                                          
070000     EJECT                                                                
080033 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDG801'.         
090031 01  DLI-IO-WDG801.                                                       
100030*  03  -COPY WDG801     -PRE WDG8-                                        
110021                                                                          
120000     EJECT                                                                
130033 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDG8A1'.         
140031 01  DLI-IO-WDG8A1.                                                       
150032*  03  -COPY WDG8A1     -PRE WDG8A-                                       
160021                                                                          
170000     EJECT                                                                
180000 LINKAGE SECTION.                                                         
190001*01  -COPY W0009      -PRE MSG-                                           
200000     EJECT                                                                
210030*01  -COPY W0008      -PRE WDG8-                                          
220000     05  FILLER                  PIC X.                                   
230013                                                                          
240032*01  -COPY W0008      -PRE WDG8A-                                         
250000     05  FILLER                  PIC X.                                   
260000     EJECT                                                                
270032 PROCEDURE DIVISION  USING MSG-PCB WDG8-PCB WDG8A-PCB.                    
280000 STYR SECTION.                                                            
290032     ENTRY 'DLITCBL' USING MSG-PCB WDG8-PCB WDG8A-PCB.                    
300007                                                                          
310013     PERFORM A-INIT                                                       
320013                                                                          
330032     PERFORM IMS-GN-WDG8A                                                 
340007     PERFORM UNTIL BASEN-SLUT                                             
350038       IF WS-CHKP-RAEKNARE > +29                                          
360032         MOVE WDG8A-SEQA-IDLTERM       TO W-IDLTERM-A1                    
370032         MOVE WDG8A-SEQA-TIREGDAT      TO W-TIREGDAT-A1                   
380032         MOVE WDG8A-SEQA-TIKLOCK-9KOMPL TO W-TIKLOCK-9KOMPL-A1            
390007         PERFORM IMS-CHECKPOINT                                           
400007         MOVE +0 TO WS-CHKP-RAEKNARE                                      
410032         PERFORM IMS-GU-WDG8A                                             
420007       END-IF                                                             
430032       IF (WDG8A-SEQA-IDPRTLST = 'AZ7     ' OR 'AZS     ')                
440035           AND WDG8A-SEQA-TIREGDAT > WS-TIREGDAT-SPAR                     
450019         CONTINUE                                                         
460019       ELSE                                                               
470032         MOVE WDG8A-SEQA-IDLTERM       TO W-IDLTERM                       
480032         MOVE WDG8A-SEQA-TIREGDAT      TO W-TIREGDAT                      
490032         MOVE WDG8A-SEQA-TIKLOCK-9KOMPL TO W-TIKLOCK-9KOMPL               
500030         PERFORM IMS-GHU-WDG8                                             
510019         PERFORM IMS-DELETE                                               
520019         ADD +1 TO WS-CHKP-RAEKNARE                                       
530019         ADD +1 TO WS-KVANTAL                                             
540019       END-IF                                                             
550032       PERFORM IMS-GN-WDG8A                                               
560007     END-PERFORM                                                          
570013                                                                          
580013     PERFORM Z-FINIT                                                      
590000                                                                          
600000     MOVE ZERO TO RETURN-CODE                                             
610000     GOBACK                                                               
620000     .                                                                    
630000     EJECT                                                                
640014 A-INIT SECTION.                                                          
650013     MOVE WHEN-COMPILED TO W-COMPILED                                     
660013     OPEN INPUT STYRIN                                                    
670013                                                                          
680013     READ STYRIN INTO IN-STYRAREA                                         
690013       AT END                                                             
700013         MOVE JA TO STYR-EOF                                              
710013         MOVE 003 TO IN-KVARBDAG                                          
720013         DISPLAY ' STYR INFO SAKNAS, 3 DAGAR ANTAGITS'                    
730013       NOT AT END                                                         
740013         DISPLAY ' INKORT = ' IN-STYRAREA                                 
750013     END-READ                                                             
760013                                                                          
770013     IF IN-KVARBDAG NOT NUMERIC                                           
780013       MOVE 003 TO IN-KVARBDAG                                            
790013       DISPLAY ' STYR EJ NUMERISK, 3 DAGAR ANTAGITS'                      
800013     END-IF                                                               
810013                                                                          
820013     ACCEPT WS-DATE FROM DATE                                             
830025     MOVE +003        TO WORK-KDCALL                                      
840027     MOVE WC-CDC-SE   TO WORK-IDDC                                        
850025     MOVE WS-DATE     TO WORK-TIAAMMDD-TOM                                
860025     MOVE 006         TO WORK-KVWORKD                                     
870025     CALL WORKDAY  USING WORK-KDCALL,                                     
880025                         WORK-DATE-AREA,                                  
890025                         WORK-KDSVAR                                      
900025     MOVE WORK-TIAAMMDD-FOM TO WS-TIREGDAT-SPAR                           
910025     MOVE IN-KVARBDAG TO WORK-KVWORKD                                     
920025     CALL WORKDAY  USING WORK-KDCALL,                                     
930025                         WORK-DATE-AREA,                                  
940025                         WORK-KDSVAR                                      
950025     IF WORK-KDSVAR-FEL                                                   
960025       MOVE 'FEL FRÅN WORKDAY I SECTION INIT' TO FELTEXT                  
970021       CALL FELLOG USING RKOD-ABEND                                       
980013     ELSE                                                                 
990026       MOVE WORK-TIAAMMDD-FOM TO W-TIREGDAT-ASEQ                          
000026     END-IF                                                               
010026                                                                          
020026     PERFORM IMS-RESTART                                                  
030026     .                                                                    
040026     EJECT                                                                
050026 Z-FINIT SECTION.                                                         
060026                                                                          
070026     CLOSE STYRIN                                                         
080026                                                                          
090013     DISPLAY ' ANTAL BORTTAGNA RÖTTER = ' WS-KVANTAL                      
100025             ' DATUM <= '  WORK-TIAAMMDD-FOM                              
110017     DISPLAY '                          '                                 
120015     .                                                                    
130013     EJECT                                                                
140000* --- IMS SEKTIONER ---                                                   
150011                                                                          
160000 IMS-RESTART SECTION.                                                     
170000                                                                          
180000     MOVE SPACE TO MSG-IO-AREA                                            
190000     MOVE '  ' TO GODK-STATUSKODER                                        
200000     CALL CBLTDLI USING XRST MSG-PCB                                      
210000                             MSG-IO-AREA-LENGTH MSG-IO-AREA               
220000                             CHKP-AREA-1-LENGTH CHKP-AREA-1               
230000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
240000     PERFORM IMS-STATUSKONTROLL                                           
250000     .                                                                    
260020                                                                          
270020                                                                          
280000 IMS-CHECKPOINT SECTION.                                                  
290000                                                                          
300000     MOVE CHKP-ID TO MSG-IO-AREA                                          
310000     MOVE '  XD' TO GODK-STATUSKODER                                      
320000     CALL CBLTDLI USING CHKP MSG-PCB                                      
330000                             MSG-IO-AREA-LENGTH MSG-IO-AREA               
340000                             CHKP-AREA-1-LENGTH CHKP-AREA-1               
350000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
360000     PERFORM IMS-STATUSKONTROLL                                           
370000     IF IMS-EJ-OK                                                         
380000       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
390000       CALL FELLOG                                                        
400000     END-IF                                                               
410000     .                                                                    
420000     EJECT                                                                
430032 IMS-GN-WDG8A SECTION.                                                    
440011                                                                          
450032     STRING 'WDG8A1  (TIREGDAT=<' W-TIREGDAT-X ')'                        
460000             DELIMITED BY SIZE INTO SSA1                                  
470000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
480034     CALL CBLTDLI USING GN WDG8A-PCB DLI-IO-WDG8A1 SSA1                   
490032     MOVE WDG8A-STATUS-CODE TO STATUS-WS                                  
500000     PERFORM IMS-STATUSKONTROLL                                           
510000     .                                                                    
520020                                                                          
530032 IMS-GU-WDG8A SECTION.                                                    
540011                                                                          
550033     STRING 'WDG8A1  (WDG8A1KY =' W-WDG8A1KY-X ')'                        
560000             DELIMITED BY SIZE INTO SSA1                                  
570000     MOVE '  ' TO GODK-STATUSKODER                                        
580033     CALL CBLTDLI USING GU WDG8A-PCB DLI-IO-WDG8A1 SSA1                   
590032     MOVE WDG8A-STATUS-CODE TO STATUS-WS                                  
600000     PERFORM IMS-STATUSKONTROLL                                           
610000     .                                                                    
620020                                                                          
630030 IMS-GHU-WDG8 SECTION.                                                    
640011                                                                          
650030     STRING 'WDG801  (WDG801KY =' W-WDG801KY-X ')'                        
660000             DELIMITED BY SIZE INTO SSA1                                  
670000     MOVE '  ' TO GODK-STATUSKODER                                        
680030     CALL CBLTDLI USING GHU WDG8-PCB DLI-IO-WDG801 SSA1                   
690030     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
700000     PERFORM IMS-STATUSKONTROLL                                           
710000     .                                                                    
720020                                                                          
730000 IMS-DELETE       SECTION.                                                
740011                                                                          
750000     MOVE '    ' TO GODK-STATUSKODER                                      
760033     CALL CBLTDLI USING DLET WDG8-PCB DLI-IO-WDG801                       
770030     MOVE WDG8-STATUS-CODE TO STATUS-WS                                   
780000     PERFORM IMS-STATUSKONTROLL                                           
790000     .                                                                    
800021                                                                          
810000     EJECT                                                                
820000 IMS-STATUSKONTROLL SECTION.                                              
830000                                                                          
840000     SET STATUS-IX TO 1                                                   
850000     SEARCH GODK-STATUS                                                   
860000       AT END                                                             
870000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
880000         DELIMITED BY SIZE INTO FELTEXT                                   
890000         CALL FELLOG                                                      
900001       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
910001         CONTINUE                                                         
920000     END-SEARCH                                                           
930000     .                                                                    
