001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W6129000.                                                
001200 AUTHOR.         MARTIEN HOMPES.                                          
001300 DATE-WRITTEN.   97/04/25.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNCTION:                                                            
001700*        LIST FREE LOCATION                                               
001800*                                                                         
001910*        THE PROGRAM UPDATES   WLLOCA (WDJ8)                              
002000*                                                                         
002100*    ABENDCODES:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- LOCATION FILE                                              
003210     SELECT W61290                     ASSIGN TO W61290D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W61290                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  POST -COPY W61290 -PRE  W61290-  -L.                                 
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W6129000'.            
004300 77  YES                         PIC X       VALUE 'Y'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004700     EJECT                                                                
004800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES TODAYS-DATE.                                        
005000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005200     03  TODAYS-DATE-DAY         PIC 9(2).                                
005300     EJECT                                                                
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     SKIP2                                                                
006100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006700 01  ERRTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
006900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007202 01  LOC-AREA-START              PIC X(24)   VALUE                        
007203                                 'LOC-AREA-START  '.                      
007204     SKIP2                                                                
007205                                                                          
007210*01  AREA -COPY W61290 -PRE UT-                                           
007300     EJECT                                                                
007400*    --- AREAS FOR IMS-SECTIONS                                           
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  KEYS-TILL-DLI.                                                       
008001     03  W-WDJ8KEY-X.                                                     
008010         05  W-WDJ8KEY           PIC S9(11)   VALUE ZERO COMP-3.          
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FOUND                       VALUE '  '.                  
008500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008540     88  END-OF-DATABASE                     VALUE 'GB'.                  
008600     SKIP2                                                                
008700 01  GOOD-STATUSCODES.                                                    
008800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008900     SKIP3                                                                
009000 01  SSA1                        PIC X(64).                               
009100 01  SSA2                        PIC X(64).                               
009200     EJECT                                                                
009300*    --- IMS FUNCTION CODES                                               
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009820 01  FILLER         PIC X(20) VALUE 'WLLOCA01-AREA'.                      
009830 01  DLI-IO-WLLOCA01.                                                     
009840*    03  -COPY WDJ801                                                     
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010300                                                                          
010401     EJECT                                                                
010402*01  -COPY W0008  -PRE LOCA-                                              
010410     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010601 PROCEDURE DIVISION  USING LOCA-PCB.                                      
010602 MAIN SECTION.                                                            
010610     ENTRY 'DLITCBL' USING LOCA-PCB.                                      
010700                                                                          
010900                                                                          
011000     PERFORM A-INIT                                                       
011100                                                                          
011201     PERFORM IMS-GET-LOCA                                                 
011202       PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE                   
011203         EVALUATE LOCA-SEG-NAME-FB                                        
011204         WHEN 'WDJ801'                                                    
011206              MOVE LOC-IDDC     TO  UT-IDDC                               
011207              MOVE LOC-ADLAGOMR TO  UT-ADLAGOMR                           
011208              MOVE LOC-ADGANG   TO  UT-ADGANG                             
011209              MOVE LOC-ADPLATS  TO  UT-ADPLATS                            
011210              MOVE LOC-KDLOC    TO  UT-KDLOC                              
011211              MOVE LOC-KDFREQ   TO  UT-KDFREQ                             
011212              MOVE LOC-KDSTOR   TO  UT-KDSTOR                             
011213              MOVE LOC-KVMPART  TO  UT-KVMPART                            
011214              MOVE LOC-TELOC    TO  UT-TELOC                              
011215              PERFORM S11-WRITE-W61290                                    
011217         END-EVALUATE                                                     
011218         PERFORM IMS-GET-LOCA                                             
011220       END-PERFORM                                                        
011300     PERFORM Z-FINIT                                                      
011400                                                                          
011500     MOVE ZERO TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 A-INIT SECTION.                                                          
012101                                                                          
012110     OPEN OUTPUT W61290                                                   
012200                                                                          
012210     MOVE '090' TO UT-IDPTYP                                              
012220                                                                          
012300     ACCEPT TODAYS-DATE  FROM DATE                                        
012410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012600     .                                                                    
012700     EJECT                                                                
012800 Z-FINIT SECTION.                                                         
012910     CLOSE W61290                                                         
013001     SKIP2                                                                
013002     MOVE 'S' TO POSTSUM-OPKOD                                            
013010     CALL POSTSUM USING POSTSUM-PARM                                      
013100     .                                                                    
013301     EJECT                                                                
013302 S11-WRITE-W61290 SECTION.                                                
013303                                                                          
013304     WRITE W61290-POST FROM UT-W61290                                     
013305                                                                          
013306     MOVE UT-IDPTYP  TO POSTSUM-TRANSTYP                                  
013307     MOVE 'W61290'   TO POSTSUM-FDNAMN                                    
013308     MOVE 'W61290D1' TO POSTSUM-DDNAMN2                                   
013309     CALL POSTSUM USING POSTSUM-PARM                                      
013310     .                                                                    
013500     EJECT                                                                
013600 S99-ABEND SECTION.                                                       
013700                                                                          
013801     SKIP2                                                                
013802     MOVE 'S' TO POSTSUM-OPKOD                                            
013810     CALL POSTSUM USING POSTSUM-PARM                                      
013900     CALL ABEND USING RKOD-ABEND                                          
014000     .                                                                    
014100     EJECT                                                                
014200* --- IMS SECTIONS  ---                                                   
014300     SKIP3                                                                
014401     EJECT                                                                
014402 IMS-GET-LOCA   SECTION.                                                  
014403                                                                          
014404     MOVE '  GAGKGBGE' TO GOOD-STATUSCODES                                
014405     CALL CBLTDLI USING GN LOCA-PCB LOC-WDJ801                            
014406     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
014408     PERFORM IMS-STATUSCHECK                                              
014410     .                                                                    
014500     EJECT                                                                
014600 IMS-STATUSCHECK SECTION.                                                 
014700                                                                          
014800     SET STATUS-IX TO 1                                                   
014900     SEARCH GOOD-STATUS                                                   
015000       AT END                                                             
015100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015200           DELIMITED BY SIZE INTO ERRTEXT                                 
015400         CALL FELLOG                                                      
015500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
015600         CONTINUE                                                         
015700     END-SEARCH                                                           
015800     .                                                                    
