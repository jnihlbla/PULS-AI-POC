//W116J191 JOB (640W1160100W116J191,W100),'RTN W116D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W116    EXEC W116P091,                                                        
//             INDIN=W116.W116D1,                                               
//             INDUT=W116.W116D1                                                
//*                                                                             
//EMPTY  EXEC WEMPTST,DSIN=W116.W116D1.W11691(+1)                               
//    IF (EMPTY.T.RC = 0) THEN                                                  
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D1.W11691(+1)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SPAREPARTINFO                                          
/*                                                                              
//*   VCOM-D&P                                                                  
// EXEC WZ14DAP4,DSIN=&&W11691                                                  
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J191                                         
