//W116J088 JOB (640W1160100W116J088,W100),'RTN W116SF',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*   VCOM=&VCOM                                                                
//*   COUNTRYX2= &COUNTRYX2                                                     
//*                                                                             
//W116    EXEC W116P088                                                         
//SORT01.SYSIN DD *                                                             
 SORT FIELDS=(1,5,PD,A),EQUALS                                                  
 INCLUDE COND=(6,2,CH,EQ,C'&COUNTRYX2')                                         
 SUM  FIELDS=(NONE)                                                             
 END                                                                            
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W116.W116SF.W11688(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116SF.W11688(+1)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
/*                                                                              
//*   VCOM-D&P                                                                  
// EXEC WZ14DAP4,DSIN=&&W11688                                                  
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J088                                         
