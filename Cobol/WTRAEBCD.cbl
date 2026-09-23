000001*COMPOPT STDSUB=YES                                                       
000002 ID DIVISION.                                                             
000003                                                                          
000004 PROGRAM-ID.             WTRAEBCD.                                        
000005 AUTHOR.                 KJELL AND UMESH                                  
000006     DATE-WRITTEN.       NOV 2011.                                        
000007*                                                                         
000008*                                                                         
000009*    FUNCTION:                                                            
000010*      TRANSLATE TEXT FROM UNICODE UTF8 TO SOME EBCDIC CODEPAGE           
000011*      OR KEEP IT IN UNICODE. IF UNICODE SHOULD BE KEPT, TRAILING         
000012*      UNICODE SPACES ARE CHANGED TO EBCDIC SPACE.                        
000013*                                                                         
000014*      THE INPUT TEXT MAY ALTERNATIVELY BE CODED AS A                     
000015*      HEXADECIMAL TEXT STRING REPRESENTING UTF8 CHARACTERS               
000016*      THAT SHOULD BE CONVERTED TO REAL UTF8 CHARACTER FORMAT.            
000017*                                                                         
000018*    CALLED BY:                                                           
000019*          CALL WTRAEBCD USING TRAEBCD-AREA                               
000020*                                                                         
000021*    INPUT ARGUMENTS:                                                     
000022*          TRAEBCD-KDCP            OUTPUT CODE PAGE                       
000023*          TRAEBCD-TECONV-FROM     INPUT TEXT.                            
000024*                                                                         
000025*    OUTPUT ARGUMENTS:                                                    
000026*          TRAEBCD-TECONV-TO       CONVERTED TEXT                         
000027*                                                                         
000028*    EXAMPLES OF POSSIBLE INPUT CODE PAGES:                               
000029*        278 - EBCDIC CODEPAGE 278 (SWEDISH EBCDIC)                       
000030*        930 - EBCDIC CODEPAGE 930 (JAPANESE EBCDIC)                      
000031*        935 - EBCDIC CODEPAGE 930 (CHINESE EBCDIC)                       
000032*        UTF8- UNICODE TRANSMISSION FORMAT                                
000035*                                                                         
000036     EJECT                                                                
000037                                                                          
000038 DATA DIVISION.                                                           
000039 WORKING-STORAGE SECTION.                                                 
000040                                                                          
000041 77  PROGRAM-NAMN            PIC X(8)  VALUE 'WTRAEBCD'.                  
000042                                                                          
000043 77  CP-TO                   PIC 9(5).                                    
000044 77  UTF8                    PIC 9(5)  VALUE 1208.                        
000045                                                                          
000046*-- THE TEXT FIELDS IN THE LINK AREA ARE 700 BYTES LONG                   
000047 77  LTEXT                   PIC S9(4) BINARY VALUE 701.                  
000048 77  UCS-TEXT                PIC N(700).                                  
000051                                                                          
000052*-- FIELDS USED WHEN CALLING SUBROUTINE HEXCONV                           
000053 01  HEXCONV                 PIC X(8)  VALUE 'HEXCONV '.                  
000054                                                                          
000055*-- CONVERSION TYPE 0=TO HEX DIGITS, 1=FROM HEX DIGITS TO TEXT            
000056 01  HC-TYPE                 PIC X     VALUE '1'.                         
000057                                                                          
000058*-- LENGTH OF FIELD TO BE CONVERTED                                       
000059*-- NOTE: HEXCONV CAN NOT HANDLE 700 BYTES, SO THE LENGTH                 
000060*-- HERE IS MADE SHORTER. HOPEFULLY WE WILL NOT HAVE LONGER               
000061*-- HEX DATA THAN 400 DIGITS (200 CHARACTERS)                             
000062 01  HC-LENGTH               PIC S9(4) BINARY VALUE +400.                 
000063                                                                          
000064*-- THE OUTPUT OF HEXCONV WHEN CONVERTING HEX->CHARACTER                  
000065 01  HC-CHAR-VALUE          PIC X(200).                                   
000066                                                                          
000067 LINKAGE SECTION.                                                         
000068                                                                          
000069 01  LINK-AREA.                                                           
000070*    03 -COPY WTRAEBCD                                                    
000071                                                                          
000072                                                                          
000073 PROCEDURE DIVISION  USING  LINK-AREA.                                    
000074                                                                          
000075*    -- FIRST, CONVERT FROM HEX STRING TO UTF-8 CHARACTERS                
000076     MOVE 400 TO HC-LENGTH                                                
000078     CALL HEXCONV USING HC-TYPE HC-LENGTH                                 
000079                        TRAEBCD-TECONV-FROM                               
000080                        HC-CHAR-VALUE                                     
000081                                                                          
000093                                                                          
000094     EVALUATE TRAEBCD-KDCP                                                
000095     WHEN 'UTF8'                                                          
000096*      -- ENSURE THAT THE TEXT IS PADDED WITH SPACE                       
000097*      -- NOT LOW-VALUE (FROM HEXCONV)                                    
000098       MOVE ZERO TO TALLY                                                 
000099       INSPECT FUNCTION REVERSE(HC-CHAR-VALUE)                            
000100         TALLYING TALLY FOR LEADING LOW-VALUE                             
000101       IF TALLY > ZERO                                                    
000102         INSPECT HC-CHAR-VALUE                                            
000103           CONVERTING LOW-VALUE TO SPACE                                  
000104       END-IF                                                             
000105                                                                          
000106       MOVE HC-CHAR-VALUE TO TRAEBCD-TECONV-TO                            
000109                                                                          
000121                                                                          
000122     WHEN OTHER                                                           
000123*      -- THEN, ENSURE THAT THE TEXT IS PADDED WITH UNICODE               
000124*      -- SPACE, NOT LOW-VALUE (FROM HEXCONV)                             
000125       MOVE ZERO TO TALLY                                                 
000126       INSPECT FUNCTION REVERSE(HC-CHAR-VALUE)                            
000127         TALLYING TALLY FOR LEADING LOW-VALUE                             
000128       IF TALLY > ZERO                                                    
000129         INSPECT HC-CHAR-VALUE                                            
000130           CONVERTING LOW-VALUE TO X'20'                                  
000131       END-IF                                                             
000132                                                                          
000133*      -- CONVERT FROM UNICODE TO EBCDIC                                  
000134*      -- THE GIVEN CODE-PAGE SHOULD BE A NUMERIC VALUE                   
000135*      -- CORREPSONDING TO THE EBCDIC CODEPAGE OR CCSID                   
000136       COMPUTE CP-TO  = FUNCTION NUMVAL(TRAEBCD-KDCP)                     
000137                                                                          
000138*      -- FIRST CONVERT TEXT TO UNICODE UCS2 FORMAT                       
000139       MOVE FUNCTION NATIONAL-OF(HC-CHAR-VALUE, UTF8)                     
000140            TO UCS-TEXT                                                   
000141*      -- THEN CONVERT IT TO EBCDIC                                       
000142       MOVE FUNCTION DISPLAY-OF(UCS-TEXT, CP-TO)                          
000143            TO TRAEBCD-TECONV-TO                                          
000144     END-EVALUATE                                                         
000145                                                                          
000146                                                                          
000147     MOVE ZERO TO RETURN-CODE                                             
000150     GOBACK                                                               
000200     .                                                                    
