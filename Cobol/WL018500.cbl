000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL018500.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/11/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.ORDERPLANNINGPRC'                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SHOWS SUBTOTALS PER SENDING WITH FOLLOWING INPUT KEYS:           
001100*        IDSHIPM AND IDDC                                                 
001200*        ---------------------------------------------------------        
001300*        PROGRAM SHOWS BOTH PACKAGE INFORMATION BEFORE SHIPPING           
001400*        AND AFTER SHIPPING DEPENDING ON THE INPUT KEYS AS BELOW          
001500*        1. IDSHIPM  GIVEN - INFORMATION AFTER SHIPPING                   
001600*        2. IDTRPTNR GIVEN - INFORMATION BEFORE SHIPPING                  
001700*        BOTH IDSHIPM AND IDTRPTNR TOGETHER ARE NOT ALLOWED               
001800*        INFORMATION SHOWN ON SCREEN -                                    
001900*        SUMMERY FIELDS: TOT NO CASE,TOT WEIGHT,TOT VOLUME,TOT            
002000*        VALUE,TOT NETWEIGHT                                              
002100*        LINE FIELDS:IDKUNDNR,IDORDER,CASE,CUSTOMS-ID,LENGTH,             
002200*        HIGHT,WEIGHT,VOLUME,VALUE,NET WEIGHT AND DANGEROUS GOODS         
002300*                                                                         
002400*          THE PROGRAM READS     WDE1                                     
002500*          THE PROGRAM READS     WDE6                                     
002600*          THE PROGRAM READS     WDB2                                     
002700*          THE PROGRAM READS     WDB1                                     
002800*          THE PROGRAM READS     WDG2                                     
002900*          THE PROGRAM READS     WDE4                                     
003000*                                                                         
003100*        WL018500 PROGRAM IS A REPLICA OF W4065400 PROGRAM                
003200*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
003300*                                                                         
003400*    INDATA.                                                              
003500*        TRANSACTION: WL0185T / WLA185                                    
003600*        REQUEST:     WL0185I1                                            
003700*                                                                         
003800*    OUTDATA.                                                             
003900*        RESPONSE:    WL0185O1                                            
004000                                                                          
004100                                                                          
004200 ENVIRONMENT DIVISION.                                                    
004300                                                                          
004400 DATA DIVISION.                                                           
004500                                                                          
004600 WORKING-STORAGE SECTION.                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'WL018500'.            
004800                                                                          
004900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005000 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005100 77  KDRC-DISPLAY                PIC Z(5).                                
005200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005300                                                                          
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600                                                                          
005700 01  ALL-PLUS.                                                            
005800     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
005900                                                                          
006000 77  IX                          PIC S9(4)   VALUE +0   COMP SYNC.        
006100 77  MAX-IX                      PIC S9(4)   VALUE +500 COMP SYNC.        
006200                                                                          
006300 01  SMALL-LETTERS              PIC X(31)  VALUE                          
006400     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖüÉ'.                                   
006500 01  CAPS-LETTERS               PIC X(31)  VALUE                          
006600     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
006700                                                                          
006800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006900 01  GENERAL-SUBPROGRAMS.                                                 
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007300     03  WL018510                PIC X(8)    VALUE 'WL018510'.            
007400                                                                          
007500*    --- PARAMETERS TO ABEND                                              
007600                                                                          
007700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008000*                                                                         
008100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008200*01  -COPY WZ01SUB                                                        
008300                                                                          
008400 01  FILLER                      PIC X(16)   VALUE 'REQU-IN-AREA'.        
008500                                                                          
008600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008700 01  REQU-AREA.                                                           
008800*    03  -COPY WZ01REQ2                                                   
008900*    03  -COPY WL0185I1                                                   
009000                                                                          
009100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009200 01  RESP-AREA.                                                           
009300*    03  -COPY WZ01RES2                                                   
009400*    03  -COPY WL0185O1                                                   
009500                                                                          
009600 LINKAGE SECTION.                                                         
009700 01  MSG-PCB                     PIC X.                                   
009800                                                                          
009900*01  -COPY W0008  -PRE WDQ3F-                                             
010000     05  FILLER                  PIC X.                                   
010100                                                                          
010200*01  -COPY W0008  -PRE WDB6-                                              
010300     05  FILLER                  PIC X.                                   
010400                                                                          
010500*01  -COPY W0008  -PRE WDE6-                                              
010600     05  FILLER                  PIC X.                                   
010700                                                                          
010800 PROCEDURE DIVISION  USING MSG-PCB WDQ3F-PCB WDB6-PCB WDE6-PCB.           
010900                                                                          
011000 MAIN SECTION.                                                            
011100     ENTRY 'DLITCBL' USING MSG-PCB WDQ3F-PCB WDB6-PCB WDE6-PCB.           
011200                                                                          
011300                                                                          
011400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
011500     IF SUB-KDRC = 0                                                      
011600       PERFORM A-INIT                                                     
011700                                                                          
011800       CALL WL018510          USING REQU-AREA RESP-AREA                   
011900                                    WDQ3F-PCB WDB6-PCB WDE6-PCB           
012000                                                                          
012100       PERFORM Z-FINIT                                                    
012200       PERFORM S02-RETURN-RESPONSE                                        
012300     END-IF                                                               
012400                                                                          
012500     MOVE ZERO                   TO RETURN-CODE                           
012600     GOBACK                                                               
012700     .                                                                    
012800                                                                          
012900 A-INIT SECTION.                                                          
013000                                                                          
013100     MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY)                             
013200                                 TO REQU-IDDC-KEY                         
013300     INSPECT REQU-IDPRC-FR-IN-KEY                                         
013400         CONVERTING SMALL-LETTERS TO CAPS-LETTERS                         
013500     INSPECT REQU-IDPRC-TO-IN-KEY                                         
013600         CONVERTING SMALL-LETTERS TO CAPS-LETTERS                         
013700     .                                                                    
013800                                                                          
013900 Z-FINIT SECTION.                                                         
014000                                                                          
014100     CONTINUE                                                             
014200     .                                                                    
014300                                                                          
014400*    --- DISPATCHER SECTIONS                                              
014500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
014600                                                                          
014700     MOVE 'GETARG'               TO SUB-KDFUNC                            
014800     MOVE 'CARPARTS.LDC.ORDERPLANNINGPRC'                                 
014900                                 TO SUB-ADDISPABS                         
015000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
015100                                                                          
015200     CALL WZ01SUB             USING SUB-CONTROL-AREA                      
015300                                    SUB-KVDLEN                            
015400                                    REQU-AREA                             
015500     IF SUB-KDRC > 0                                                      
015600       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
015700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
015800             DELIMITED BY SIZE INTO ERROR-TEXT                            
015900       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
016000     END-IF                                                               
016100     .                                                                    
016200     SKIP3                                                                
016300 S02-RETURN-RESPONSE SECTION.                                             
016400                                                                          
016500     MOVE 'RETURN'               TO SUB-KDFUNC                            
016600     MOVE LENGTH OF RESP-AREA    TO SUB-KVDLEN                            
016700                                                                          
016800     CALL WZ01SUB             USING SUB-CONTROL-AREA                      
016900                                    SUB-KVDLEN                            
017000                                    RESP-AREA                             
017100                                                                          
017200     IF SUB-KDRC > 0                                                      
017300       MOVE SUB-KDRC             TO KDRC-DISPLAY                          
017400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
017500             DELIMITED BY SIZE INTO ERROR-TEXT                            
017600       CALL ABEND             USING RKOD-ABEND-WITH-DUMP                  
017700     END-IF                                                               
017800     .                                                                    
017900                                                                          
