000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W510PRDC.                                                
000400 AUTHOR.         SARASWATHY S.                                            
000500 DATE-WRITTEN.   20/04/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SUBPROGRAM TO READ ALL DC THAT HAVE THE SAME IDLEGSEL            
001100*        FROM WDB601                                                      
001200*                                                                         
001300*                                                                         
001400*    ABENDCODES:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
001910 ENVIRONMENT DIVISION.                                                    
001920     SKIP2                                                                
001930 INPUT-OUTPUT SECTION.                                                    
001940                                                                          
001950 FILE-CONTROL.                                                            
001960     EJECT                                                                
001970 DATA DIVISION.                                                           
001980     SKIP2                                                                
001990 FILE SECTION.                                                            
001991     EJECT                                                                
001992 WORKING-STORAGE SECTION.                                                 
001993                                                                          
001994 77  IDPGM                       PIC X(8)    VALUE 'W510PRDC'.            
001995 77  YES                         PIC X       VALUE 'J'.                   
001996 77  NOO                         PIC X       VALUE 'N'.                   
001997 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
001998 77  WS-IX                       PIC 9(3)    VALUE ZERO.                  
001999 77  WS-IX-MAX                   PIC 9(3)    VALUE 50.                    
002000     EJECT                                                                
002010 01  GENERAL-SUBPROGRAMS.                                                 
002011*                                                                         
002012     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
002013     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
002014     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
002015     SKIP2                                                                
002016*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
002017                                                                          
002018 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
002019 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
002020 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
002021     SKIP2                                                                
002022 01  ERROR-TEXT.                                                          
002023     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
002024     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
002025     EJECT                                                                
002027*    --- AREAS FOR IMS-SECTIONS                                           
002028*                                                                         
002029     EJECT                                                                
002030 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
002031     SKIP3                                                                
002032 01  KEYS-FOR-DLI.                                                        
002033     03  W-IDLEGSEL-X.                                                    
002034         05  W-IDLEGSEL          PIC X(4)   VALUE SPACES.                 
002035     SKIP2                                                                
002036*    --- STATUS-KOD FRÅN IMS                                              
002037 01  STATUS-WS                   PIC XX.                                  
002038     88  SEGMENT-FOUND                       VALUE '  '.                  
002039     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
002040     88  SEGMENT-MISSING                     VALUE 'GE'.                  
002041     88  SEGMENT-END-OF-DB                   VALUE 'GB'.                  
002042     SKIP2                                                                
002043 01  GOOD-STATUSCODES.                                                    
002044     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
002045     SKIP3                                                                
002046 01  SSA1                        PIC X(64).                               
002047 01  SSA2                        PIC X(64).                               
002048     EJECT                                                                
002049*    --- IMS FUNCTION CODES                                               
002050*01  -COPY W0003                                                          
002051     EJECT                                                                
002052*    ---  DLI INPUT-OUTPUT AREA                                           
002053 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
002054 01  DLI-IO-WDB601.                                                       
002055*    03  -COPY WDB601                                                     
002056     EJECT                                                                
002065 LINKAGE SECTION.                                                         
002066     SKIP2                                                                
002067 01  W510PRDC-AREA.                                                       
002068*    03  -COPY W510PRDC                                                   
002069     EJECT                                                                
002070*01  -COPY W0008  -PRE WDB6-                                              
002071     05  FILLER                  PIC X.                                   
002072     EJECT                                                                
002073 PROCEDURE DIVISION  USING W510PRDC-AREA  WDB6-PCB.                       
002074 MAIN SECTION.                                                            
002075                                                                          
002076     ACCEPT DAGENS-DATUM  FROM DATE                                       
002077                                                                          
002078     EVALUATE PRDC-KDCALL                                                 
002079                                                                          
002080       WHEN 010                                                           
002082          PERFORM A-INIT                                                  
002083          PERFORM B-READ-WDB601                                           
002084       WHEN OTHER                                                         
002085*** ERROR CODE ON KDCALL NOT AVAILABLE                                    
002086          ADD 1 TO WS-IX                                                  
002087          MOVE SPACE      TO PRDC-IDDC(WS-IX)                             
002088     END-EVALUATE                                                         
002089                                                                          
002090     MOVE ZERO TO RETURN-CODE                                             
002091     GOBACK                                                               
002092     .                                                                    
002093     EJECT                                                                
002094                                                                          
002095 A-INIT SECTION.                                                          
002096     PERFORM VARYING WS-IX FROM +1 BY +1                                  
002098       UNTIL WS-IX > WS-IX-MAX                                            
002099       MOVE SPACES TO PRDC-IDDC(WS-IX)                                    
002100     END-PERFORM                                                          
002101     INITIALIZE WS-IX                                                     
002102     .                                                                    
002103     EJECT                                                                
002104                                                                          
002105 B-READ-WDB601 SECTION.                                                   
002108     MOVE PRDC-IDLEGSEL TO W-IDLEGSEL                                     
002109     PERFORM IMS-GN-WDB601-LEGSEL                                         
002110     PERFORM UNTIL SEGMENT-END-OF-DB                                      
002111       IF SEGMENT-FOUND                                                   
002112         IF DCS-IDLEGSEL = PRDC-IDLEGSEL AND WS-IX < WS-IX-MAX            
002113           ADD 1 TO WS-IX                                                 
002115           MOVE DCS-IDDC TO PRDC-IDDC(WS-IX)                              
002116           MOVE '1'      TO PRDC-KDSVAR                                   
002117         END-IF                                                           
002118       END-IF                                                             
002119       PERFORM IMS-GN-WDB601-LEGSEL                                       
002120     END-PERFORM                                                          
002121     .                                                                    
002130     EJECT                                                                
002217* --- IMS SECTIONS ---                                                    
002218     SKIP3                                                                
002219 IMS-GN-WDB601-LEGSEL SECTION.                                            
002220     STRING 'WDB601  (IDLEGSEL =' W-IDLEGSEL ')'                          
002221            DELIMITED BY SIZE INTO SSA1                                   
002222     MOVE '  GB'                 TO GOOD-STATUSCODES                      
002223     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
002224     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
002225     PERFORM IMS-STATUSCHECK                                              
002226     .                                                                    
002227     SKIP3                                                                
002264 IMS-STATUSCHECK SECTION.                                                 
002265                                                                          
002266     SET STATUS-IX TO 1                                                   
002267     SEARCH GOOD-STATUS                                                   
002268       AT END                                                             
002269         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
002270           DELIMITED BY SIZE INTO ERROR-TEXT                              
002271         DISPLAY ERROR-TEXT                                               
002272         CALL FELLOG                                                      
002273       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
002274         CONTINUE                                                         
002275     END-SEARCH                                                           
002276     .                                                                    
