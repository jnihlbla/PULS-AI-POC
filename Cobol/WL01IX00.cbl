000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     WL01IX00.                                                
000003 AUTHOR.         ANDRE KJELL.                                             
000004 DATE-WRITTEN.   11/11/01.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    NAME:       CARPARTS.LDC.APPLINDEX                                   
000008*                                                                         
000009*    FUNCTION:                                                            
000010*        FETCH DC INFO TO THE LDC/NDC APPLICATION                         
000011*                                                                         
000012*        THE PROGRAM READS     WDB6                                       
000013*                                                                         
000014*    INDATA.                                                              
000015*        TRANSACTION: WL01IXT                                             
000016*        REQUEST:     WL01IXI1                                            
000017*                                                                         
000018*    OUTDATA.                                                             
000019*        RESPONSE:    WL01IXO1                                            
000020                                                                          
000021     EJECT                                                                
000022 DATA DIVISION.                                                           
000023                                                                          
000024 WORKING-STORAGE SECTION.                                                 
000025 77  IDPGM                       PIC X(08)   VALUE 'WL01IX00'.            
000026                                                                          
000027*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
000028 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
000029 77  KDRC-DISPLAY                PIC Z(5).                                
000030                                                                          
000031 77  YES                         PIC X       VALUE 'J'.                   
000032 77  NOO                         PIC X       VALUE 'N'.                   
000033                                                                          
000034                                                                          
000035 77  KEYS-SW                     PIC X       VALUE 'J'.                   
000036     88  KEYS-OK                             VALUE 'J'.                   
000037     88  KEYS-WRONG                          VALUE 'N'.                   
000038     EJECT                                                                
000039*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
000040 01  GENERAL-SUBPROGRAMS.                                                 
000041     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000042     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000043     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
000044     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000045                                                                          
000046*    --- PARAMETERS TO ABEND                                              
000047                                                                          
000048 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000049 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
000050 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
000051     EJECT                                                                
000052*                                                                         
000053 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
000054                                                                          
000055*01  -COPY WZ01SUB                                                        
000056                                                                          
000057     EJECT                                                                
000058 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
000059                                                                          
000060 01  REQU-AREA.                                                           
000061*    03  -COPY WZ01REQU                                                   
000062*    03  -COPY WL01IXI1                                                   
000063     EJECT                                                                
000064                                                                          
000065 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
000066     SKIP3                                                                
000067 01  RESP-AREA.                                                           
000068*    03  -COPY WZ01RESP                                                   
000069*    03  -COPY WL01IXO1                                                   
000070                                                                          
000071     EJECT                                                                
000072*    --- WORK-AREAS FOR IMS-SECTIONS                                      
000073*                                                                         
000074 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000075                                                                          
000076 01  KEYS-FOR-DLI.                                                        
000077     03  W-IDDC                  PIC X(2)    VALUE SPACE.                 
000078                                                                          
000079*    --- STATUS-KOD FRÅN IMS                                              
000080 01  STATUS-WS                   PIC XX.                                  
000081     88  SEGMENT-FOUND                       VALUE '  '.                  
000082     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
000083     88  SEGMENT-MISSING                     VALUE 'GE'.                  
000084                                                                          
000085 01  GOOD-STATUSCODES.                                                    
000086     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000087                                                                          
000088 01  SSA1                        PIC X(64).                               
000089     EJECT                                                                
000090*    --- IMS FUNCTION CODES                                               
000091*01  -COPY W0003                                                          
000092                                                                          
000093     EJECT                                                                
000094                                                                          
000095 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
000096 01  DLI-IO-WDB601.                                                       
000097*    03  -COPY WDB601                                                     
000098                                                                          
000099     EJECT                                                                
000100 LINKAGE SECTION.                                                         
000101                                                                          
000102*01  -COPY W0009  -PRE MSG-                                               
000103                                                                          
000104*01  -COPY W0008  -PRE WDB6-                                              
000105     05  FILLER                  PIC X.                                   
000106                                                                          
000107     EJECT                                                                
000108 PROCEDURE DIVISION  USING MSG-PCB WDB6-PCB.                              
000109 MAIN SECTION.                                                            
000110     ENTRY 'DLITCBL' USING MSG-PCB WDB6-PCB.                              
000111                                                                          
000112     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
000113     IF SUB-KDRC = 0                                                      
000114       PERFORM A-INIT                                                     
000115       PERFORM B-CHECK-KEYS                                               
000116       IF KEYS-OK                                                         
000117         PERFORM F-READ-SHOW-INFO                                         
000118       END-IF                                                             
000119       PERFORM S02-RETURN-RESPONSE                                        
000120     END-IF                                                               
000121                                                                          
000122     MOVE ZERO TO RETURN-CODE                                             
000123     GOBACK                                                               
000124     .                                                                    
000125                                                                          
000126     EJECT                                                                
000127 A-INIT       SECTION.                                                    
000128                                                                          
000129     MOVE SPACE TO RESP-AREA                                              
000130     MOVE '001' TO RESP-IDMSGVER                                          
000131     .                                                                    
000132                                                                          
000133     EJECT                                                                
000134 B-CHECK-KEYS SECTION.                                                    
000135                                                                          
000136*    -- ALL VALUES OF IDDC ARE ACCEPTED                                   
000137     MOVE YES TO KEYS-SW                                                  
000138     .                                                                    
000139                                                                          
000140     EJECT                                                                
000141 F-READ-SHOW-INFO SECTION.                                                
000142                                                                          
000143     PERFORM FA-READ-BASICDATA                                            
000144                                                                          
000145     IF SEGMENT-MISSING                                                   
000146*       -- DC NOT FOUND. RETURN ERROR AND SPACE IN ALL FIELDS             
000148        MOVE '025'        TO RESP-IDMSG-ERROR                             
000150        MOVE 'IDDC'       TO RESP-IDELMT-ERROR                            
000151        MOVE SPACE        TO RESP-DCS-WL01IXO1                            
000152     ELSE                                                                 
000153        MOVE DCS-IDLANDX2 TO RESP-DCS-IDLANDX2                            
000154        MOVE DCS-IDTIDZON TO RESP-DCS-IDTIDZON                            
000155        MOVE DCS-KDDC     TO RESP-DCS-KDDC                                
000156        MOVE DCS-IDFTG    TO RESP-DCS-IDFTG                               
000157        MOVE DCS-FLINVACS TO RESP-DCS-FLINVACS                            
000158     END-IF                                                               
000159     .                                                                    
000160                                                                          
000161     EJECT                                                                
000162 FA-READ-BASICDATA SECTION.                                               
000163                                                                          
000164     MOVE REQU-IDDC-KEY TO W-IDDC                                         
000165     PERFORM IMS-GET-WDB601                                               
000166     .                                                                    
000167                                                                          
000168     EJECT                                                                
000169*    --- DISPATCHER SECTIONS                                              
000170 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
000171                                                                          
000172     MOVE 'GETARG'               TO SUB-KDFUNC                            
000173     MOVE 'CARPARTS.LDC.APPLINDEX' TO SUB-ADDISPABS                       
000174     MOVE LENGTH OF REQU-AREA      TO SUB-KVDLEN                          
000175                                                                          
000176     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
000177                                                                          
000178     IF SUB-KDRC > 0                                                      
000179       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
000180       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
000181       DELIMITED BY SIZE INTO ERROR-TEXT                                  
000182       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
000183     END-IF                                                               
000184     .                                                                    
000185     SKIP3                                                                
000186 S02-RETURN-RESPONSE SECTION.                                             
000187                                                                          
000188     MOVE 'RETURN'                   TO SUB-KDFUNC                        
000189     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
000190                                                                          
000191     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
000192                                                                          
000193     IF SUB-KDRC > 0                                                      
000194       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
000195       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
000196       DELIMITED BY SIZE INTO ERROR-TEXT                                  
000197       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
000198     END-IF                                                               
000201     .                                                                    
000202     EJECT                                                                
000203 IMS-GET-WDB601 SECTION.                                                  
000204                                                                          
000205     STRING 'WDB601  (IDDC     =' W-IDDC   ')'                            
000206          DELIMITED BY SIZE INTO SSA1                                     
000207     MOVE '  GE' TO GOOD-STATUSCODES                                      
000208     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
000209     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
000210     PERFORM IMS-STATUSCHECK                                              
000211     .                                                                    
000212     SKIP3                                                                
000213 IMS-STATUSCHECK    SECTION.                                              
000214                                                                          
000215     SET STATUS-IX TO 1                                                   
000216     SEARCH GOOD-STATUS                                                   
000217       AT END                                                             
000218         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
000219         DELIMITED BY SIZE INTO ERROR-TEXT                                
000220         CALL FELLOG                                                      
000221       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
000222         CONTINUE                                                         
000223     END-SEARCH                                                           
000230     .                                                                    
