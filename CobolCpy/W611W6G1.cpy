001000*** EDIT ALLOWED                                                          
020000*      CONTROL COPYTEXT FOR VALID IDDC IN SYSTEM W611 FOR                 
021000*                       INBOUND LEAD TIME CALCULATION.                    
040003 77  W6GX01-LT-IDDC         PIC X(2).                                     
050003     88 GOOD-LT-IDDC        VALUES ARE                                    
130003           '11' '12' '71' '72' '73' '74'.                                 
130103                                                                          
131003 01  LT-IX-MAX              PIC S9(4)   COMP-3 VALUE +6.                  
131103                                                                          
132102 01  LT-IDDC.                                                             
132202     03 LT-IDDC-VALUES.                                                   
133002        05 FILLER  PIC X(2)  VALUE '11'.                                  
134002        05 FILLER  PIC X(2)  VALUE '12'.                                  
134003        05 FILLER  PIC X(2)  VALUE '71'.                                  
134004        05 FILLER  PIC X(2)  VALUE '72'.                                  
134005        05 FILLER  PIC X(2)  VALUE '73'.                                  
134006        05 FILLER  PIC X(2)  VALUE '74'.                                  
139103                                                                          
139104     03 LT-IDDC-TABELL    REDEFINES LT-IDDC-VALUES.                       
139105        05 LT-DC    OCCURS 6   PIC X(2).                                  
140000                                                                          
