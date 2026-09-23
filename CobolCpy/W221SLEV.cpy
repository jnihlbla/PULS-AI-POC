000010*** EDIT ALLOWED                                                          
000001***********************************************************               
000002*    TABELLER ÖVER VISSA LEVERANTÖRER (NORDISKA + EUROPEISKA)             
000003*    SOM SKALL HA SINA AVROP JUSTERADE VID SEMESTERTID                    
000004*    ( OBS  FÖREKOMMER I BÅDE PGM W22140 OCH W22150)                      
000005***********************************************************               
000006*-----------------------------------------  TABELL-INTERVALL              
000007*                        SVENSKA, DANSKA OCH NORSKA LEVERANTÖRER          
000008 01  NO-TABELL.                                                           
000009     03  FILLER              PIC  X(10)  VALUE '1    1001 '.              
000010     03  FILLER              PIC  X(10)  VALUE '1003 2699 '.              
000011     03  FILLER              PIC  X(10)  VALUE '7000 7199 '.              
000012     03  FILLER              PIC  X(10)  VALUE '7300 7599 '.              
000013     03  FILLER              PIC  X(10)  VALUE '1000112699'.              
000014     03  FILLER              PIC  X(10)  VALUE '1700017199'.              
000015     SKIP1                                                                
000016 01  FILLER              REDEFINES NO-TABELL.                             
000017     03  FILLER          OCCURS 6.                                        
000018         05  NO-IDLEVNR-FOM  PIC X(5).                                    
000019         05  NO-IDLEVNR-TOM  PIC X(5).                                    
000020                                                                          
000021 01  MAX-IX-NO-IDLEVNR       PIC S9(3)  COMP-3  VALUE +6.                 
000022                                                                          
000023*-----------------------------------------  TABELL-INTERVALL              
000024*                        EUROPEISKA LEVERANTÖRER                          
000025 01  EU-TABELL.                                                           
000026     03  FILLER              PIC  X(10)  VALUE '3300 3999 '.              
000027     03  FILLER              PIC  X(10)  VALUE '4451 4964 '.              
000028     03  FILLER              PIC  X(10)  VALUE '5000 6999 '.              
000029     03  FILLER              PIC  X(10)  VALUE '7200 7299 '.              
000030     03  FILLER              PIC  X(10)  VALUE '7750 7824 '.              
000031     03  FILLER              PIC  X(10)  VALUE '1330013499'.              
000032     03  FILLER              PIC  X(10)  VALUE '1445114759'.              
000033     03  FILLER              PIC  X(10)  VALUE '1775017824'.              
000034     SKIP1                                                                
000035 01  FILLER              REDEFINES EU-TABELL.                             
000036     03  FILLER          OCCURS 8.                                        
000037         05  EU-IDLEVNR-FOM  PIC X(5).                                    
000038         05  EU-IDLEVNR-TOM  PIC X(5).                                    
000040                                                                          
000100 01  MAX-IX-EU-IDLEVNR       PIC S9(3)  COMP-3  VALUE +8.                 
