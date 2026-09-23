000010*** EDIT ALLOWED                                                          
000100 01  W271AKT.                                                             
000200*                                 TABELL SOM ANGER ANTAL ORDER-           
000300*                                 TRÄFFAR SOM KRÄVS FÖR ATT               
000400*                                 AKTIVERA RESP. PASSIVERA EN             
000700*                                 ARTIKEL PÅ ETT S-LAGER.                 
000710*                                                                         
000800     03 DC21-ANTAL-ORDERTRAEFFAR.                                         
000900        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
000901        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001440*                                                                         
001441     03 DC23-ANTAL-ORDERTRAEFFAR.                                         
001442        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001443        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001444*                                                                         
001450     03 DC24-ANTAL-ORDERTRAEFFAR.                                         
001460        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001470        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001480*                                                                         
001490     03 DC25-ANTAL-ORDERTRAEFFAR.                                         
001491        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001492        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001493*                                                                         
001494     03 DC26-ANTAL-ORDERTRAEFFAR.                                         
001495        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001496        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001497*                                                                         
001498     03 DC41-ANTAL-ORDERTRAEFFAR.                                         
001499        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001500        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001501*                                                                         
001502     03 DC42-ANTAL-ORDERTRAEFFAR.                                         
001503        05 FILLER       PIC X(14) VALUE '000005000 0401'.                 
001504        05 FILLER       PIC X(14) VALUE '999999999 0401'.                 
001505*                                                                         
001506     03 DC43-ANTAL-ORDERTRAEFFAR.                                         
001507        05 FILLER       PIC X(14) VALUE '000005000 0401'.                 
001508        05 FILLER       PIC X(14) VALUE '999999999 0401'.                 
001509*                                                                         
001510     03 DC51-ANTAL-ORDERTRAEFFAR.                                         
001511        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001512        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001513*                                                                         
001514     03 DC61-ANTAL-ORDERTRAEFFAR.                                         
001515        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001516        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001517*                                                                         
001518     03 DC62-ANTAL-ORDERTRAEFFAR.                                         
001519        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001520        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001521*                                                                         
001522     03 DC1A-ANTAL-ORDERTRAEFFAR.                                         
001523        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001524        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001525*                                                                         
001522     03 DC1B-ANTAL-ORDERTRAEFFAR.                                         
001523        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001524        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001525*                                                                         
001522     03 DC2A-ANTAL-ORDERTRAEFFAR.                                         
001523        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001524        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001525*                                                                         
001441     03 DC3A-ANTAL-ORDERTRAEFFAR.                                         
001442        05 FILLER       PIC X(14) VALUE '000005000 0201'.                 
001443        05 FILLER       PIC X(14) VALUE '999999999 0201'.                 
001444*                                                                         
001526 01  FILLER REDEFINES W271AKT.                                            
001527     03 DC-AKTPARM       OCCURS 15.                                       
001528         05 DC-PARM          OCCURS 2.                                    
001530             07 DC-PRARTSTD-MAX    PIC 9(7)V9(2).                         
001540             07 FILLER            PIC X.                                  
001550             07 DC-KVOT-AKT        PIC 9(2).                              
001570             07 DC-KVOT-PASS       PIC 9(2).                              
001800*                                                                         
003600*** END COPY W271AKT     LENGTH=86                                        
