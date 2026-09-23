000010 01  WXTR3A.                                                              
000020*                                 PRIMARY EXTRACT                         
000030*                                                                         
000040*                                 PARTNO INFORMATION                      
000050*                                                                         
000060     03 IDARTNR              PIC S9(9)           COMP-3.                  
000070*                                 PART NUMBER                             
000080     03 BEART-SVE            PIC X(25).                                   
000090     03 BEART-ENG            PIC X(25).                                   
000100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
000110*                                 PRODUCT GROUP                           
000120     03 BEPRODSL             PIC X(15).                                   
000130*                                 TYPE OF ASSORTMENT DESCRIPTION          
000140     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
000150*                                 FUNCTION GROUP                          
000160     03 BEFKNGRP             PIC X(50).                                   
000170*                                 FUNCTIONAL GROUP NAME                   
000180     03 IDLEVNR              PIC S9(5)           COMP-3.                  
000190*                                 SUPPLIER NUMBER                         
000200     03 IDANSK               PIC S9(3)           COMP-3.                  
000210*                                 PROCURER NO.                            
000220     03 KDVVKL               PIC S9              COMP-3.                  
000230*                                 VOLUME VALUE CLASS                      
000240     03 IDLKTO               PIC S9(7)           COMP-3.                  
000250*                                 STOCK ACCOUNT (CCMMMSS)                 
      *** END COPY WXTR3A      LENGTH=135                                       
