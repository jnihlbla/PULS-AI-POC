//W116J045 JOB (640W1160100W116J045,W100),'RTN W116D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//EMPTY   EXEC WEMPTST,DSIN=W116.W116D3.W11647(+0)                              
//*                                                                             
//   IF (EMPTY.T.RC = 4) THEN                                                   
//DEL   EXEC PGM=IEFBR14                                                        
//DD    DD DSN=W116.W116D3.W11647(+0),DISP=(OLD,DELETE)                         
//   ELSE                                                                       
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=AT                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=BE                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=CH                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=CZ                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=DE                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=DK                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=ES                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=FI                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=FR                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=GB                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=HU                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=IE                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=IT                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=NL                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=NO                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=PL                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=PT                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//*                                                                             
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W116.W116D3.W11647(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.SUPERSESSIONINFO                                       
¤MQMPROP Market=SE                                                              
¤MQMPROP Vidb_Source=Delta                                                      
¤MQMPROP LoadType=Delta                                                         
/*                                                                              
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W116J045                                         
