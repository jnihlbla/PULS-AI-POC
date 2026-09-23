000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W3715500.                                                
000301 AUTHOR.         HÅKAN BOHLIN.                                            
000401 DATE-WRITTEN.   19/11/29.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701                                                                          
000801*    FUNCTION:                                                            
001002*        DELETE PRINTED REPORTS ON EVENT DATABASE.                        
001102*        SYMBOLIC PARAMETERS IDUSER/IDDC/IDDC-REC.                        
001202*        THIS PROGRAM IS INCLUDED IN A ROUTINE WHICH                      
001302*        CREATE CORE EXPORT DOCUMENT.                                     
001402*                                                                         
001502*        PROGRAM    UPDATE   WDR2 (WDGX3148/WDGX3150)                     
001602*                                                                         
001702                                                                          
001802     SKIP3                                                                
001902 ENVIRONMENT DIVISION.                                                    
002002     SKIP2                                                                
002102 INPUT-OUTPUT SECTION.                                                    
002202                                                                          
002302 FILE-CONTROL.                                                            
002402     SKIP2                                                                
002502*          --- INPUT PARAMETERS FROM CORE RECEIVING SCREEN                
002602     SELECT INDATA                     ASSIGN TO W37155D1.                
002702                                                                          
002802 DATA DIVISION.                                                           
002902     SKIP3                                                                
003002 FILE SECTION.                                                            
003102     SKIP3                                                                
003202 FD  INDATA                                                               
003302     RECORDING       F                                                    
003402     BLOCK CONTAINS  0.                                                   
003502     SKIP2                                                                
003602 01  IN-RECORD            PIC X(80).                                      
003702     EJECT                                                                
003802 WORKING-STORAGE SECTION.                                                 
003902                                                                          
004002 77  IDPGM                       PIC X(8)    VALUE 'W3715500'.            
004102 77  YES                         PIC X       VALUE 'J'.                   
004202 77  NOO                         PIC X       VALUE 'N'.                   
004302 01  CHKP-VAR.                                                            
004402     03  CHKP-MSG-IO-AREA-LENGTH  PIC S9(9) VALUE +32 COMP SYNC.          
004502     03  CHKP-MSG-IO-AREA         PIC X(32) VALUE SPACE.                  
004602     03  CHKP-AREA-LENGTH         PIC S9(9) VALUE +32 COMP SYNC.          
004702     03  CHKP-AREA                PIC X(32) VALUE SPACE.                  
004802     SKIP2                                                                
004902 01  ERRTEXT.                                                             
005002     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005102     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005202     EJECT                                                                
005302                                                                          
005402 01  DYNAMISKA-SUBPROGRAM.                                                
005502*                                                                         
005602     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005702     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005802     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005902     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006102*                                                                         
006202*01  -COPY W0005   -PRE  POSTSUM-                                         
006302     EJECT                                                                
006402                                                                          
006502 01  IN-AREA-START               PIC X(16)   VALUE                        
006602                                 'IN-AREA-START  '.                       
006702     SKIP2                                                                
006802 01  IN-AREA-1.                                                           
006903     03  IN-IDUSER               PIC X(8).                                
007103     03  FILLER                  PIC X(72).                               
007104                                                                          
007105     SKIP2                                                                
007106 01  IN-AREA-2.                                                           
007108     03  IN-IDDC                 PIC X(2).                                
007109     03  FILLER                  PIC X(78).                               
007110                                                                          
007120     SKIP2                                                                
007130 01  IN-AREA-3.                                                           
007140     03  IN-IDDC-REC             PIC X(2).                                
007150     03  FILLER                  PIC X(78).                               
007503                                                                          
007603     EJECT                                                                
007703 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007803     SKIP3                                                                
007903 01  KEYS-TO-DLI.                                                         
008003     03   W-WDGXKEY-3147-X.                                               
008103         05  W-IDHTYP            PIC X(4)   VALUE '3147'.                 
008203         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
008303                                                                          
008403     03  W-WDGXKEY-3148-X.                                                
008503         05 W-IDUSER             PIC X(8)  VALUE SPACE.                   
008603         05 W-IDDC               PIC X(2)  VALUE SPACE.                   
008604         05 W-IDDC-REC           PIC X(2)  VALUE SPACE.                   
008703     EJECT                                                                
008803*    --- STATUS-CODE FROM IMS                                             
008903 01  STATUS-WS                   PIC XX.                                  
009003     88  SEGMENT-EXIST                       VALUE '  '.                  
009103     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009203     SKIP2                                                                
009303 01  GOOD-STATUSCODES.                                                    
009403     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009503     SKIP3                                                                
009603 01  SSA1                        PIC X(64).                               
009703 01  SSA2                        PIC X(64).                               
009803     EJECT                                                                
009903*    --- IMS RETURNCODES                                                  
010003*01  -COPY W0003                                                          
010103     EJECT                                                                
010203*    ---  DLI INPUT-OUTPUT AREA                                           
010303 01  FILLER                PIC X(16)   VALUE 'DLI-IO-3148'.               
010403 01  DLI-IO-3148.                                                         
010503*    03  -COPY WDGX3148                                                   
010603     EJECT                                                                
010703 LINKAGE SECTION.                                                         
010803                                                                          
010903*01  -COPY W0009  -PRE MSG-                                               
011003     EJECT                                                                
011103*01  -COPY W0008  -PRE 3147-                                              
011203     05  FILLER                  PIC X.                                   
011303     EJECT                                                                
011403 PROCEDURE DIVISION  USING MSG-PCB 3147-PCB.                              
011503 MAIN SECTION.                                                            
011603     ENTRY 'DLITCBL' USING MSG-PCB 3147-PCB.                              
011703                                                                          
011803     SKIP2                                                                
011903     PERFORM A-INIT                                                       
012003                                                                          
012103     MOVE IN-IDUSER         TO W-IDUSER                                   
012203     MOVE IN-IDDC           TO W-IDDC                                     
012204     MOVE IN-IDDC-REC       TO W-IDDC-REC                                 
012303     PERFORM IMS-GHU-WDGX3148                                             
012403     IF SEGMENT-EXIST                                                     
012503        PERFORM IMS-DLET-WDGX3148                                         
012603     END-IF                                                               
012703                                                                          
012803     MOVE ZERO TO RETURN-CODE                                             
012903     GOBACK                                                               
013003     .                                                                    
013103     EJECT                                                                
013203 A-INIT SECTION.                                                          
013303                                                                          
013403     PERFORM IMS-RESTART                                                  
013503                                                                          
013603     OPEN INPUT INDATA                                                    
013703     READ INDATA INTO IN-AREA-1                                           
013704     READ INDATA INTO IN-AREA-2                                           
013705     READ INDATA INTO IN-AREA-3                                           
013903     END-READ                                                             
014003     CLOSE INDATA                                                         
014103                                                                          
014203     MOVE IDPGM             TO POSTSUM-PROGNAMN                           
014303     .                                                                    
014403     EJECT                                                                
014503* --- IMS SECTIONS ---                                                    
014603                                                                          
014703 IMS-GHU-WDGX3148 SECTION.                                                
014803                                                                          
014903     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-3147-X ')'                    
015003          DELIMITED BY SIZE INTO SSA1                                     
015103     STRING 'WDGX3148(KY3148   =' W-WDGXKEY-3148-X ')'                    
015203          DELIMITED BY SIZE INTO SSA2                                     
015303     MOVE '  GE' TO GOOD-STATUSCODES                                      
015403     CALL CBLTDLI USING GHU 3147-PCB DLI-IO-3148 SSA1 SSA2                
015503     MOVE 3147-STATUS-CODE TO STATUS-WS                                   
015603     PERFORM IMS-STATUSCHECK                                              
015703     .                                                                    
015803     SKIP2                                                                
015903 IMS-DLET-WDGX3148 SECTION.                                               
016003                                                                          
016103     MOVE '  ' TO GOOD-STATUSCODES                                        
016203     CALL CBLTDLI USING DLET 3147-PCB DLI-IO-3148                         
016303     MOVE 3147-STATUS-CODE TO STATUS-WS                                   
016403     PERFORM IMS-STATUSCHECK                                              
016503     .                                                                    
016603     EJECT                                                                
016703 IMS-RESTART SECTION.                                                     
016803                                                                          
016903     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
017003     MOVE '  ' TO GOOD-STATUSCODES                                        
017103     CALL CBLTDLI USING XRST MSG-PCB                                      
017203                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
017303                        CHKP-AREA-LENGTH CHKP-AREA                        
017403     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
017503     PERFORM IMS-STATUSCHECK                                              
017603     .                                                                    
017703     SKIP2                                                                
017803 IMS-STATUSCHECK SECTION.                                                 
017903     SKIP2                                                                
018003     SET STATUS-IX TO 1                                                   
018103     SEARCH GOOD-STATUS                                                   
018203       AT END                                                             
018303         STRING ' WRONG STATUS CODE FROM IMS: ' STATUS-WS                 
018403           DELIMITED BY SIZE INTO ERRTEXT-STR                             
018503         DISPLAY ERRTEXT                                                  
018603         CALL FELLOG                                                      
018703       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
018803         CONTINUE                                                         
018903     END-SEARCH                                                           
019003     .                                                                    
