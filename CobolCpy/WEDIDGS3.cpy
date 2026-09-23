000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI DGS DANGEROUS GOODS                                              
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000031*                                                                         
000033*    FUNCTION,                                                            
000034*    -TO INDICATE THE CLASS OF DANGEROUS GOODS.                           
000036*                                                                         
000040*                                                                         
000100 01  WEDIDGS3.                                                            
000230     03 DGS3-IDPTYP                            PIC X(03).                 
000240*                                              DGS                        
000501     03 DGS3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 097               
000503*                                                                         
000515     03 DGS3-8273-DANGEROUS-GOODS-REG          PIC X(03).                 
000516*                                                                         
000517     03 DGS3-C205-HAZARD-CODE.                                            
000518*                                                                         
000519        05 DGS3-8351-HAZARD-CODE-ID            PIC X(07).                 
000520*                                                                         
000521        05 DGS3-8078-HAZARD-ITEM-PAGE-NO       PIC X(07).                 
000522*                                                                         
000523        05 DGS3-8092-HAZARD-CODE-VER-NO        PIC X(10).                 
000524*                                                                         
000525*                                                                         
000526     03 DGS3-C234-UNDG-INFORMATION.                                       
000527*                                                                         
000528        05 DGS3-7124-UNDG-NUMBER               PIC 9(04).                 
000529*                                                                         
000530        05 DGS3-7088-DANG-GOODS-FLASHP         PIC X(07).                 
000531*                                                                         
000532*                                                                         
000533     03 DGS3-C223-DANG-GOODS-SHIP-FLP.                                    
000534*                                                                         
000535        05 DGS3-7106-SHIPMENT-FLASHP           PIC 9(03).                 
000536*                                                                         
000537        05 DGS3-6411-MEASURE-UNIT-QUAL         PIC X(07).                 
000538*                                                                         
000539*                                                                         
000540     03 DGS3-8339-PACKING-GROUP-CODED          PIC X(03).                 
000541*                                                                         
000550     03 DGS3-8364-EMS-NUMBER                   PIC X(06).                 
000563*                                                                         
000564     03 DGS3-8410-MFAG                         PIC X(04).                 
000565*                                                                         
000566     03 DGS3-8126-TREM-CARD-NUMBER             PIC X(10).                 
000567*                                                                         
000568     03 DGS3-C235-HAZARD.                                                 
000569*                                                                         
000570        05 DGS3-8158-HAZ-ID-NO-UPPER           PIC X(04).                 
000571*                                                                         
000572        05 DGS3-8186-SUBST-ID-NO-LOWER         PIC X(04).                 
000573*                                                                         
000574*                                                                         
000575     03 DGS3-C236-DANG-GOODS-LABEL.                                       
000576*                                                                         
000577        05 DGS3-82461-DANG-GOODS-LABEL-1       PIC X(04).                 
000578*                                                                         
000579        05 DGS3-82462-DANG-GOODS-LABEL-2       PIC X(04).                 
000580*                                                                         
000581        05 DGS3-82463-DANG-GOODS-LABEL-3       PIC X(04).                 
000582*                                                                         
000583*                                                                         
000584     03 DGS3-8255-PACKING-INSTRUCTION          PIC X(03).                 
000585*                                                                         
000586     03 DGS3-8325-CAT-MEANS-OF-TRPT            PIC X(03).                 
000587*                                                                         
000588     03 DGS3-8211-PERMISSION-FOR-TRPT          PIC X(03).                 
000589*                                                                         
000590*                                                                         
000600*** END OF VILMAII-COPY LENGTH=103                                        
