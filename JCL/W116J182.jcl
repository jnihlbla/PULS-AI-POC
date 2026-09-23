//W116J182 JOB (640W1160100W116J182,W100),'RTN W116D6',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST3                                                    
//      INCLUDE MEMBER=SYSTÖ                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W116    EXEC W116P082,                                                        
//             DSIN=W116.W116D6.W11624(+0),                                     
//             DSUT=W116.W116D6.W11682(+1)                                      
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W116.W116D6.W11682(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D6.W11682(+1)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SPAREPARTINFO                                          
/*                                                                              
//*   VCOM-D&P                                                                  
//VCOM    EXEC WZ14DAP4,DSIN=&&W11682                                           
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J182                                         
